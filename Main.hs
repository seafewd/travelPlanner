{-
  Authors:
    Alexander Korpas
    Robin Karlsson
    Andreas Hålén
-}

module Main where

import System.Environment
import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import qualified Data.PSQueue as PSQ
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

-- read arguments, build graph, output shortest path
main :: IO ()
main = do
  args <- getArgs
  let stopsFile = args !! 0
  let linesFile = args !! 1
  let startNode = args !! 2
  let endNode   = args !! 3
  Right lines <- readLines linesFile
  let graph = buildGraph empty $ getLineInfo lines
  let path = shortestPath graph startNode endNode
  print $ snd $ fromJust path
  putStr $ unlines $ fst $ fromJust path


-- runGUI start method
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "input/stops-gbg.txt"
  Right lines <- readLines "input/lines-gbg.txt"
  let graph = buildGraph empty (getLineInfo lines)
  runGUI stops lines graph shortestPath


-- get stops, consisting of a list of tuples (String, Integer) 
getLineInfo :: [LineTable] -> [(String, Integer)]
getLineInfo lt = tuples
    where
        lineStops = [stops line | line <- lt]
        tuples = [(stopName stops, time stops) | stops <- concat lineStops]


-- build a graph using a list of LineStop
-- if a source or destination doesn't already exist in the map, add it as key
-- if source or dest. exists - add an edge from that node to another
-- if nextTime (the next value in the list) is 0, skip that row
buildGraph :: (Eq b, Num b) => Ord a => Graph a b -> [(a, b)] -> Graph a b
buildGraph (Graph m) [] = Graph m
buildGraph (Graph m) [_] = Graph m
buildGraph g ((name, time):snd@(nextName, nextTime):rest)
    | nextTime == 0 = buildGraph g (snd:rest)
    | otherwise = buildGraph (addEdge (Edge name nextName nextTime) g) ((nextName, nextTime):rest)

-- create shortest path between two nodes and display weight
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph start end =
  let
    -- start node with weight 0 in first pass
    pq = PSQ.insert (start, start) 0 PSQ.empty
    path = Just []
    map = shortestPath' graph start pq M.empty
  in
    Just (fromJust $ buildShortestPath start end path map, snd $ fromJust $ M.lookup end map)

-- use a map to create the shortest path between two nodes
buildShortestPath :: (Eq a, Ord a) => a -> a -> Maybe [a] -> M.Map a (a, b) -> Maybe [a]
buildShortestPath current next path map
  | isNothing path = Nothing
  | current == next = Just $ next : fromJust path
  | otherwise = buildShortestPath current prevNode path' map
  where
    -- node leading to the current node
    prevNode = fst $ fromJust (M.lookup next map)
    -- update path to include node path and total weight
    path' = Just $ next : fromJust path

{-
-- use a map to create the shortest path between two nodes
-- super scuffed but it works
buildShortestPath :: (Num b, Eq a, Ord a, Ord b) => a -> a -> a -> Maybe ([a], b) -> M.Map a (a, b) -> Maybe ([a], b)
buildShortestPath current next end path map
  | isNothing path = Nothing
  | current == next = Just (next : fst (fromJust path), snd $ fromJust $ M.lookup end map)
  | otherwise = buildShortestPath current prevNode end path' map
  where
    -- node leading to the current node
    prevNode = fst $ fromJust (M.lookup next map)
    -- update path to include node path and total weight
    path' = Just (next : fst (fromJust path), snd $ fromJust $ M.lookup end map)
-}

-- shortestPath' graph "A" (PSQ.insert ("A", "A") 0 PSQ.empty) M.empty
shortestPath' :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ.PSQ (a, a) b -> M.Map a (a, b) -> M.Map a (a, b)
shortestPath' graph node pq map
  -- all nodes explored, return map of paths
  | PSQ.null pq = map
  -- if destination node of minimum element in pq is already in map, delete it and run again
  | snd (PSQ.key (fromJust (PSQ.findMin pq))) `M.member` map = shortestPath' graph node (PSQ.deleteMin pq) map
  -- otherwise we have a new destination node
  | otherwise =
    let
      -- get source node of the tuple in the key
      sourceNode = fst (PSQ.key (fromJust (PSQ.findMin pq)))
      -- get destination node
      destNode = snd (PSQ.key (fromJust (PSQ.findMin pq)))
      -- get weight
      sourceWeight = PSQ.prio (fromJust (PSQ.findMin pq))
      -- update map
      map' = M.insert destNode (sourceNode, sourceWeight) map
      -- get outgoing edges
      neighboringEdges = edges destNode graph
      -- update PQ with neighboring edges
      pq' = foldr (\(Edge src dest weight) q -> PSQ.insertWith min (src, dest) (sourceWeight + weight) q) pq neighboringEdges
    in
      -- run again with updated pq, map and the new list of neighbors that haven't already been visited
      shortestPath' graph destNode pq' map'

-- use a list of edges to insert a path from source to destination along with accumulated weight into PQ
insertPath :: (Ord a, Ord b, Num b) => [Edge a b] -> M.Map a (a, b) -> b -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
insertPath [] _ _ pq = pq
insertPath ((Edge src dest weight):es) map sourceWeight pq =
  let
    pq' = PSQ.insert (src, dest) totalWeight pq
  in insertPath es map sourceWeight pq'
  where
    totalWeight = weight + sourceWeight