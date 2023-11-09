{-# LANGUAGE NoImplicitPrelude #-}

module Util (findShortestPathLength) where

import Data.Graph (Vertex)
import Data.Tree (Tree (..))
import Relude

findShortestPathLength :: Vertex -> Tree Vertex -> [Int]
findShortestPathLength target root = execState (findShortestPath' 0 root) []
  where
    findShortestPath' :: Int -> Tree Vertex -> State [Int] ()
    findShortestPath' depth (Node vertex trees) =
        if vertex == target
            then state (\depths -> ((), depth : depths))
            else mapM_ (findShortestPath' (depth + 1)) trees
