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
testMap = M.fromList [
    ("A", [
        Edge "A" "B" 15,
        Edge "A" "C" 53
        ]
    ),
    ("B", [
        Edge "B" "A" 15,
        Edge "B" "C" 40,
        Edge "B" "D" 46
        ]
    ),
    ("C", [
        Edge "C" "A" 53,
        Edge "C" "B" 40,
        Edge "C" "E" 31,
        Edge "C" "G" 17
        ]
    ),
    ("D", [
        Edge "D" "B" 46,
        Edge "D" "E" 3,
        Edge "D" "F" 11
        ]
    ),
    ("E", [
        Edge "E" "C" 31,
        Edge "E" "D" 3,
        Edge "E" "F" 8,
        Edge "E" "G" 29
        ]
    ),
    ("F", [
        Edge "F" "D" 11,
        Edge "F" "E" 8,
        Edge "F" "G" 40
      ]
    ),
    ("G", [
        Edge "G" "C" 17,
        Edge "G" "E" 29,
        Edge "G" "F" 40
      ]
    )
    ]

graph = Graph testMap