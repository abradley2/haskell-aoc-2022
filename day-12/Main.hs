{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Exception (throw)
import Data.Attoparsec.ByteString qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as P8
import Data.Graph (Edge, Vertex)
import Data.Graph qualified as Graph
import Util qualified
import Data.List (elemIndex)
import Relude

parseInput :: P.Parser [[Char]]
parseInput = do
  c <- P.many1 (P8.notChar '\n')
  cx <- P.try (P8.endOfLine >> parseInput) <|> (P8.endOfInput >> pure [])
  pure $ c : cx

canMove :: Char -> Char -> Maybe ()
canMove fromSymbol toSymbol =
  let
    fromVal = tileVal fromSymbol
    toVal = tileVal toSymbol
   in
    (if (fromVal >= toVal) || (fromVal + 1 == toVal) then Just () else Nothing)

getVertexFor :: [[Char]] -> Char -> Vertex
getVertexFor rows target =
  getVertex (rows >>= identity) 0
  where
    getVertex chars idx =
      case chars !!? idx of
        Nothing -> getVertex chars (idx + 1)
        Just char -> if char == target then idx else getVertex chars (idx + 1)


toEdges :: [[Char]] -> Int -> Int -> [Edge]
toEdges allTiles colIdx rowIdx =
  case allTiles !!? rowIdx of
    Nothing -> []
    Just row ->
      case row !!? colIdx of
        Nothing -> if colIdx == length row then toEdges allTiles 0 (rowIdx + 1) else []
        Just tile ->
          let
            toVertex c r = (c + (r * length row))

            toEdge = (toVertex colIdx rowIdx,)

            checkDirection c r =
              (allTiles !!? r >>= (!!? c) >>= canMove tile)
                $> toEdge (toVertex c r)

            up = checkDirection colIdx (rowIdx - 1)
            down = checkDirection colIdx (rowIdx + 1)
            left = checkDirection (colIdx - 1) rowIdx
            right = checkDirection (colIdx + 1) rowIdx
           in
            catMaybes [up, down, left, right] <> toEdges allTiles (colIdx + 1) rowIdx

main :: IO ()
main = do
  input <- readFileBS "day-12/input.txt"
  let result = P.parseOnly parseInput input
  case result of
    Left err -> putStrLn $ "Error parsing input: " <> err
    Right tiles -> do
      let edges = toEdges tiles 0 0
      let startVertex = getVertexFor tiles 'S'
      let endVertex = getVertexFor tiles 'E'
      let graph = Graph.buildG (0, length tiles * maybe 0 length (tiles !!? 0)) edges
      print $ Util.findShortestPathLength endVertex <$> Graph.dfs graph [startVertex]

newtype InvalidTileValue = InvalidTileValue String deriving (Show)
instance Exception InvalidTileValue

tileVal :: Char -> Int
tileVal v = case elemIndex v "abcdefghijklmnopqrstuvwxyz" of
  Just i -> i
  Nothing ->
    case v of
      'S' -> tileVal 'a'
      'E' -> tileVal 'z'
      _ -> throw $ InvalidTileValue $ "Invalid tile value: " <> show v