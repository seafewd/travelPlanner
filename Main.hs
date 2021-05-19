module Main where

import Route
import RouteGUI
import Graph  -- Create a module and use a sensible graph representation
import qualified Data.PSQueue as PSQ


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
shortestPath :: Graph a b -> Name -> Name -> Maybe ([Name], Cost)
shortestPath = 
  let
    pq = PSQ.insert 1 2 PSQ.empty
    in undefined


-- get a tuple of (String, Integer) of stops
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


    
main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "input/stops-gbg.txt"
  Right lines <- readLines "input/lines-gbg.txt"
  let graph = buildGraph empty (getLineInfo lines) -- TODO: build your graph here using stops and lines
  print graph
  runGUI stops lines graph shortestPath