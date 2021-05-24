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
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath graph start end =
  let
    pq = PSQ.insert (start, start) 0 PSQ.empty    -- start node with weight 0 in first pass
    set = S.empty
    in buildShortestPath $ shortestPath' graph start pq set

buildShortestPath :: Num b => S.Set (a, a, b) -> Maybe ([a], b)
--buildShortestPath S.empty = Nothing
buildShortestPath s =
  let
    nodeList = [fstTriple n | n <- S.toList s]
    totalWeight = sum [thrdTriple n | n <- S.toList s]
  in Just (nodeList, totalWeight)

fstTriple :: (a, b, c) -> a
fstTriple (a,_,_) = a

thrdTriple :: (a, b, c) -> c
thrdTriple (_,_,c) = c

-- keep track of set of nodes where we've been + queue with things to visit
  -- if not empty, grab least element
      -- check if already visited, if so then remove element and call recursively
      -- if empty, visit it - add to map with cost to node
shortestPath' :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ.PSQ (a, a) b -> S.Set (a, a, b) -> S.Set (a, a, b)
shortestPath' graph currentNode pq set
  | PSQ.null pq = set          -- we explored all nodes - return the set of paths
  | otherwise =
    let
      nbs = neighbors graph currentNode                               -- neighbors of currentNode
      pq = discoverNeighbors graph currentNode $ PSQ.deleteMin pq     -- add traversal path & weight to pq
    in shortestPath' graph currentNode pq set


-- update the priority queue with newly discovered vertices
discoverNeighbors :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
discoverNeighbors g node = insertPath nEdges
  where
    nEdges = edges node g       -- get a list of edges from this node


-- use a list of edges to insert a path from source to destination along with weight into PQ
-- also add the weight
insertPath :: (Ord a, Ord b, Num b) => [Edge a b] -> PSQ.PSQ (a, a) b -> PSQ.PSQ (a, a) b
insertPath [] pq = pq
insertPath [Edge src dest weight] pq 
  = PSQ.insert (src, dest) (weight + fromMaybe 0 (PSQ.lookup (src, dest) pq)) pq                  -- insert single element
insertPath ((Edge src dest weight):es) pq 
  = insertPath es $ PSQ.insert (src, dest) (weight + fromMaybe 0 (PSQ.lookup (src, dest) pq)) pq  -- insert from list


someEdges = [
  Edge "A" "B" 7,
  Edge "A" "C" 2,
  Edge "A" "E" 10,
  Edge "C" "D" 5,
  Edge "D" "F" 11
  ]
someNodes = [
  "A",
  "B",
  "E",
  "F"
  ]


someSet = S.fromList [("A", "B", 10), ("H", "J", 8)]

somePQ = PSQ.singleton ("A", "B") 10