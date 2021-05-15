module Graph (
    Graph,
    empty, addEdge
)
where


import Data.Map (Map)
import qualified Data.Map as M

data Edge a b = Edge { 
    src :: a,
    dst :: a,
    label :: b
}   deriving Show

newtype Graph a b = Graph {
    adjMap :: Map a [Edge a b]
} deriving Show


-- get an empty graph
empty :: Graph a b
empty = Graph M.empty


-- get vertices
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap


-- add an edge to the graph
addEdge :: Edge a b -> Graph a b -> Graph a b
addEdge (Edge src dest l) (Graph a) = undefined


-- add a node to the graph
addNode :: a
addNode = undefined


-- get number of edges
nEdges :: a
nEdges = undefined


-- get nodes
nodes :: a
nodes = undefined


-- get neighbors
neighbours :: a
neighbours = undefined
