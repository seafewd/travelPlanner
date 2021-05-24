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
    nodes = vertices graph
    in buildShortestPath $ shortestPath' graph nodes pq M.empty

-- use a map to create the shortest path between two nodes
buildShortestPath :: Num b => M.Map a (a, b) -> Maybe ([a], b)
buildShortestPath m =
  let
    nodeList = M.keys m--[M.lookup n m | n <- M.toList m]
    totalWeight = sum [1,2,3]--[M.lookup n | n <- M.toList m]
  in Just (nodeList, totalWeight)


-- is this real life? OR patterns hello?
shortestPath' :: (Ord a, Ord b, Num b) => Graph a b -> [a] -> PSQ.PSQ (a, a) b -> M.Map a (a, b) -> M.Map a (a, b)
shortestPath' graph (node:rest) pq map = shortestPath'' graph (node:rest) pq map
shortestPath' graph [] pq map = shortestPath'' graph [] pq map

-- shortestPath' test
--t =  shortestPath' graph allNodes (PSQ.insert ("A", "A") 0 PSQ.empty) M.empty
shortestPath'' :: (Ord a, Ord b, Num b) => Graph a b -> [a] -> PSQ.PSQ (a, a) b -> M.Map a (a, b) -> M.Map a (a, b)
shortestPath'' graph (node:rest) pq map
  -- all nodes explored, return map of paths
  | PSQ.null pq || null rest = map
  -- otherwise keep adding unexplored nodes to the queue
  | otherwise =
    let
      -- extract minimum element from pq - [("A","B") :-> 15]
      minElement = fromJust $ PSQ.findMin pq
      -- get weight from minElement - 15
      minWeight = PSQ.prio minElement
      -- get second element of the tuple in the key. this is the destination node - "B"
      minDestinationNode = snd $ PSQ.key minElement
      -- get neighboring edges of "A"
      nEdges = edges node graph
      -- update map with minimum element - M.Map "A" ("B", 15)
      map = M.insert node (minDestinationNode, minWeight) map
      -- delete minimum element, add traversal path & weight to pq
      pq = insertPath nEdges map $ PSQ.deleteMin pq
    in shortestPath'' graph rest pq map


-- use a list of edges to insert a path from source to destination along with accumulated weight into PQ
insertPath :: (Ord a, Ord b, Num b) => [Edge a b] -> M.Map a (a, b) -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
insertPath [] _ pq = pq
-- insert single edge
insertPath [Edge src dest weight] map pq
  | src `M.member` map = pq
  | otherwise = PSQ.insert (src, dest) (weight + snd (fromJust (M.lookup src map))) pq
-- insert from list of edges
insertPath ((Edge src dest weight):es) map pq
  -- if src is already a key in map, run again with the rest of the list
  | src `M.member` map = insertPath es map $ PSQ.insert (src, dest) (weight + snd (fromJust (M.lookup src map))) pq
  | otherwise = error "SCUFFED - no path from src to destination"
--  | otherwise = insertPath es map $ PSQ.insert (src, dest) (weight + snd (fromJust (M.lookup src map))) pq



-- given a source node, destination node and a list of edges
-- find the weight for the edge in the k/v pair
lookupEdgeWeight :: (Eq a, Num b) => a -> a -> [Edge a b] -> b
lookupEdgeWeight _ _ [] = error "Missing edge!"
lookupEdgeWeight src dest (Edge src' dest' weight:es)
    | src == src' && dest == dest' = weight
    | otherwise = lookupEdgeWeight src dest es


singleMap = M.singleton "A" ("A", 0)
multiMap = M.fromList [("A", ("A", 0)), ("A", ("B", 7))]


edgesA = [
  Edge "A" "B" 7,
  Edge "A" "C" 2,
  Edge "A" "E" 10
  ]

edgesC = [
  Edge "C" "D" 5,
  Edge "C" "E" 9
  ]

edgesD = [
  Edge "D" "F" 11
  ]

someNodes = [
  "A",
  "B",
  "E",
  "F"
  ]

allNodes = vertices graph

--someMap = M.fromList [("A", "B", 7), ("A", "C", 2), ("A", "E", 10), ("B", "D", 2), ("C", "D", 5), ("C", "E", 9), ("D", "F", 11), ("E", "F", 9)]

someSet = S.fromList [("A", "B", 7), ("A", "C", 2), ("A", "E", 10), ("B", "D", 2), ("C", "D", 5), ("C", "E", 9), ("D", "F", 11), ("E", "F", 9)]

--somePQ = PSQ.fromList [("A", "B") :-> 7, ("B", "D") 2]