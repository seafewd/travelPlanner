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
-- if nextTime (the next value in the list) is 0, we skip that row (depends strongly on the format of the .txt)
buildGraph :: (Eq b, Num b) => Ord a => Graph a b -> [(a, b)] -> Graph a b
buildGraph (Graph m) [] = Graph m
buildGraph g ((name, time):snd@(nextName, nextTime):rest)
    | nextTime == 0 = buildGraph g rest
    | null snd || null rest = addEdge (Edge name name time) g
    | otherwise = buildGraph (addEdge (Edge name nextName time) g) ((nextName, nextTime):rest)



  -- let map = start node -> 0
-- while not all nodes are in map:
  -- for each node x in map and each neighbor y of x
  -- calculate d = distance to x + cost of edge from x to y
  -- find the node y which has the smallest value for d
  -- add that y and its distance s to map

-- for better time complexity: priority queue
-- save neighbors in pq along with distance
-- whenever we visit a node, add each of its unvisited neighbors to pq
-- also store previous node to be able to calculate shortest path by following references backwards to the start node

-- let map = {} and q = {start node -> 0}
-- while q is not empty
  -- remove entry (x d z) from q that has the smallest priority (distance) d. z is the node's predecessor
  -- if x is in S, do nothing
  -- else add (x d z) to S and for each outgoing edge x -> y, add (y (d + w) x) to q, where w is the weight of the edge
  
--shortestPath = undefined

shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph start end =
  let
    -- start node with weight 0 in first pass
    pq = PSQ.insert (start, start) 0 PSQ.empty
    nodes = vertices graph
    in buildShortestPath $ shortestPath' graph nodes pq M.empty


--buildShortestPath map = Just (["A"], 5)

buildShortestPath :: Num b => M.Map a (a, b) -> Maybe ([a], b)
buildShortestPath m =
  let
    nodeList = M.keys m--[M.lookup n m | n <- M.toList m]
    totalWeight = sum [1,2,3]--[M.lookup n | n <- M.toList m]
  in Just (nodeList, totalWeight)
 

fstTriple :: (a, b, c) -> a
fstTriple (a,_,_) = a

thrdTriple :: (a, b, c) -> c
thrdTriple (_,_,c) = c
 
 

shortestPath' :: (Ord a, Ord b, Num b) => Graph a b -> [a] -> PSQ.PSQ (a, a) b -> M.Map a (a, b) -> M.Map a (a, b)
shortestPath' graph (node:rest) pq map
  -- all nodes explored, return the map of paths
  | PSQ.null pq =  map
  -- otherwise keep adding unexplored nodes to the queue
  | otherwise =
    let
      -- get neighboring edges
      nEdges = edges node graph
      -- add traversal path & weight to pq
      pq = insertPath nEdges map $ PSQ.deleteMin pq
    in shortestPath' graph rest pq map


-- use a list of edges to insert a path from source to destination along with accumulated weight into PQ
insertPath :: (Ord a, Ord b, Num b) => [Edge a b] -> M.Map a (a, b) -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
insertPath [] _ pq = pq
-- insert single element
insertPath [Edge src dest weight] map pq 
  | not $ src `M.member` map = PSQ.insert (src, dest) (weight + fromMaybe 0 (PSQ.lookup (src, dest) pq)) pq
-- insert from list
insertPath ((Edge src dest weight):es) map pq 
  | not $ src `M.member` map = insertPath es map $ PSQ.insert (src, dest) (weight + fromMaybe 0 (PSQ.lookup (src, dest) pq)) pq



insertPathTest = insertPath [Edge "A" "B" 15, Edge "A" "C" 53] M.empty PSQ.empty

someEdges = [
  Edge "A" "B" 7,
  Edge "A" "C" 2,
  Edge "A" "E" 10,
  Edge "B" "D" 2,
  Edge "C" "D" 5,
  Edge "C" "E" 9,
  Edge "D" "F" 11,
  Edge "E" "F" 9
  ]
someNodes = [
  "A",
  "B",
  "E",
  "F"
  ]

--someMap = M.fromList [("A", "B", 7), ("A", "C", 2), ("A", "E", 10), ("B", "D", 2), ("C", "D", 5), ("C", "E", 9), ("D", "F", 11), ("E", "F", 9)]

someSet = S.fromList [("A", "B", 7), ("A", "C", 2), ("A", "E", 10), ("B", "D", 2), ("C", "D", 5), ("C", "E", 9), ("D", "F", 11), ("E", "F", 9)]

somePQ = PSQ.singleton ("A", "B") 10