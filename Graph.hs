module Graph (
    Graph,
    empty, addEdge
)
where

import Route
import RouteGUI
import Data.Map (Map)
import qualified Data.Map as M

data Edge a b = Edge { 
    src :: a,
    dst :: a,
    label :: b
}   deriving Show

data Graph a b = Graph {
    adjMap :: Map a [Edge a b]
} deriving Show


-- get an empty graph
empty :: Graph a b
empty = Graph M.empty


-- get vertices
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap


-- dijkstra's algorithm
dijkstra :: Ord a => a -> Graph a b -> Map a (a, b)
dijkstra = undefined


-- add an edge to the graph
addEdge :: Edge a b -> Graph a b
addEdge = undefined


-- add a node to the graph
addNode = undefined


-- get number of edges
nEdges = undefined


-- get nodes
nodes = undefined


-- get neighbors
neighbours = undefined
