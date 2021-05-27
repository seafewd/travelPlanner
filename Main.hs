{-
  Authors:
    Alexander Korpas
    Robin Karlsson
    Andreas Hålén
-}

module Main where

import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import qualified Data.PSQueue as PSQ
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = startGUI  -- TODO: read arguments, build graph, output shortest path


-- runGUI start method
startGUI :: IO ()
startGUI = do
  Right stops <- readStops "input/stops-gbg.txt"
  Right lines <- readLines "input/lines-gbg.txt"
  let graph = buildGraph empty (getLineInfo lines)
  print graph
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
buildGraph g ((name, time):snd@(nextName, nextTime):rest)
    | nextTime == 0 = buildGraph g rest
    | null snd || null rest = addEdge (Edge name name time) g
    | otherwise = buildGraph (addEdge (Edge name nextName time) g) ((nextName, nextTime):rest)


-- create shortest path between two nodes and display weight
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph start end =
  let
    -- start node with weight 0 in first pass
    pq = PSQ.insert (start, start) 0 PSQ.empty
    path = Just ([], 0)
  in
    buildShortestPath start end path $ shortestPath' graph start pq M.empty


-- use a map to create the shortest path between two nodes
buildShortestPath :: (Num b, Eq a, Ord a, Ord b) => a -> a -> Maybe ([a], b) -> M.Map a (a, b) -> Maybe ([a], b)
buildShortestPath current end path map
  | isNothing path = Nothing
  | current == end = path
  | otherwise =
  let
    -- node leading to the current node
    prevNode = fst $ fromJust (M.lookup end map)
    -- accumulated weight
    accWeight = snd (fromJust path)
    -- weight for current node
    weight = (snd $ fromJust $ M.lookup end map)
    -- update path to include node path and total weight
    path' = Just (end : fst (fromJust path), accWeight - weight)
  in
    -- run until we reach the end node
    buildShortestPath current prevNode path' map


--t = shortestPath' graph allNodes (PSQ.insert ("A", "A") 0 PSQ.empty) M.empty
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
      pq' = insertPath neighboringEdges map' sourceWeight pq
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