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
    nodes = neighbors graph start
    in buildShortestPath end $ shortestPath' graph nodes pq M.empty


buildShortestPath = undefined
{-
A, (A, 0)
C, (A, 2)
D, (C, 5)
F, (D, 11)
-- use a map to create the shortest path between two nodes
buildShortestPath :: Num b => a -> M.Map a (a, b) -> Maybe ([a], b) -> Maybe ([a], b)
buildShortestPath end map
  | null map = fromJust Nothing ([], 0)
  | otherwise =
    let
      totalWeight = sum [fromJust $ snd $ M.lookup end map | M.toList map]
      nodeList = undefined
    in
      buildShortestPath
-}


--t = shortestPath' graph allNodes "A" (PSQ.insert ("A", "A") 0 PSQ.empty) M.empty
shortestPath' :: (Ord a, Ord b, Num b) => Graph a b -> [a] -> PSQ.PSQ (a, a) b -> M.Map a (a, b) -> M.Map a (a, b)
shortestPath' _ [] _ map = map
shortestPath' graph (node:_) pq map
  -- all nodes explored, return map of paths
  | PSQ.null pq = map
  -- otherwise keep adding unexplored nodes to the queue
  | otherwise =
    let
      -- get minimum element of pq
      minElement = findMin pq map
      -- get weight from minElement
      sourceWeight = PSQ.prio minElement
      -- get source node of the tuple in the key
      sourceNode = fst $ PSQ.key minElement
      -- get neighboring edges of current node
      neighboringEdges = edges node graph
      -- update map with minimum element if it doesn't already exist in map
      map' = mapInsert node sourceNode sourceWeight map
      -- delete minimum element, add traversal path & weight to pq
      pq' = insertPath neighboringEdges map' sourceWeight $ PSQ.deleteMin pq
    in 
      shortestPath' graph (addNeighborNodes graph (neighbors graph node) map') pq' map'


-- find the minimum element of the PQ
-- check the second element of the tuple of the binding's minimum element and
-- see if it's already a member of the map. this means we've already visited this node
-- so delete it from PQ and keep checking this until we can return something that isn't already in the map
findMin :: (Ord a, Ord b) => PSQ.PSQ (a, a) b -> M.Map a (a, b) -> PSQ.Binding (a, a) b
findMin pq map
  | snd (PSQ.key (fromJust $ PSQ.findMin pq)) `M.member` map = findMin (PSQ.deleteMin pq) map
  | otherwise = fromJust $ PSQ.findMin pq


-- add neighboring nodes into list if they aren't already in the visited map
addNeighborNodes :: Ord a => Graph a b -> [a] -> M.Map a (a, b) -> [a]
addNeighborNodes graph nodes map = [n | n <- nodes, n `M.notMember` map]


-- insert into map only if key doesn't already exist
mapInsert :: Ord a => a -> a -> b -> M.Map a (a, b) -> M.Map a (a, b)
mapInsert node sourceNode weight map
  | node `M.member` map = map
  | otherwise = M.insert node (sourceNode, weight) map


-- use a list of edges to insert a path from source to destination along with accumulated weight into PQ
insertPath :: (Ord a, Ord b, Num b) => [Edge a b] -> M.Map a (a, b) -> b -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
insertPath [] _ _ pq = pq
insertPath ((Edge src dest weight):es) map sourceWeight pq =
  let
    pq' = PSQ.insert (src, dest) totalWeight pq
  in insertPath es map sourceWeight pq'
  where
    totalWeight = weight + sourceWeight
    betterPathExists = totalWeight > snd (fromJust (M.lookup src map))


-- test structs

singleMap = M.singleton "A" ("A", 0)
multiMap = M.fromList [("A", ("A", 0)), ("A", ("B", 15))]


edgesA = [
  Edge "A" "B" 15,
  Edge "A" "C" 53
  ]

edgesB = [
  Edge "B" "A" 15,
  Edge "B" "C" 40,
  Edge "B" "D" 46
  ]

edgesC = [
  Edge "C" "A" 53,
  Edge "C" "B" 40,
  Edge "C" "E" 31,
  Edge "C" "G" 17
  ]

edgesD = [
  Edge "D" "B" 46,
  Edge "D" "F" 11,
  Edge "D" "E" 3
  ]

edgesE = [
  Edge "E" "D" 3,
  Edge "E" "C" 31,
  Edge "E" "F" 8,
  Edge "E" "G" 29
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

--somePQ = PSQ.fromList [("A","B") :-> 15, ("B", "D") :-> 2]