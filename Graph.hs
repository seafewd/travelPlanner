module Graph
where

import Route
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Control.Monad


data Edge a b = Edge {
    src :: a,
    dst :: a,
    weight :: b
}   deriving Show

newtype Graph a b = Graph {
    adjMap  :: Map a [Edge a b]
} deriving Show


-- get an empty graph
empty :: Graph a b
empty = Graph M.empty


-- get vertices
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap


-- add an edge to the graph
addEdge :: Ord a => Edge a b -> Graph a b -> Graph a b
addEdge e@(Edge src dest _) (Graph m)
    | M.notMember src m = addEdge e $ Graph (M.insert src [] m)
    | M.notMember dest m = addEdge e $ Graph (M.insert dest [] m)
    | otherwise = Graph (M.insertWith (++) src [e] m)


-- based on key, get edges(value) 
edges :: Ord a => a -> Graph a b -> [Edge a b]
edges key (Graph m) = fromMaybe [] $ M.lookup key m


-- add a node to the graph
addNode :: Ord a => a -> Graph a b -> Graph a b
addNode node (Graph m) = Graph (M.insert node [] m)

-- get destination node from edge
getDestNode :: Edge a b -> a
getDestNode (Edge src dest _) = dest


-- get number of edges
--nEdges :: Graph a b -> Int
--nEdges (Graph m) = foldr (\s n -> length (M.lookup n m) s + n) 0 m


-- returns a list with all nodes adjacent to the given node in the graph
neighbors :: Ord a => Graph a b -> a -> [a]
neighbors (Graph m) key = nVertices
    where 
        nEdges = fromMaybe [] (M.lookup key m)  -- get edges of this node
        nVertices = [dst d | d <- nEdges]       -- get neighbors of node


-- test map
m = M.fromList [
    ("A", [
        Edge "A" "B" 7,
        Edge "A" "C" 2,
        Edge "A" "E" 10
        ]
    ),
    ("B", [
        Edge "B" "D" 2
        ]
    ),
    ("C", [
        Edge "C" "E" 9,
        Edge "C" "D" 5
        ]
    ),
    ("D", [
        Edge "D" "F" 11
        ]
    ),
    ("E", [
        Edge "E" "F" 9
        ]
    ),
    ("F", []
    )
    ]

graph = Graph m