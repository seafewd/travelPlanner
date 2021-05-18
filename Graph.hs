module Graph (
    Graph,
    empty, addEdge, m, graph
)
where

import qualified Data.PSQueue as PSQ
import Route
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe


data Edge a b = Edge {
    src :: a,
    dst :: a,
    label :: b
}   deriving Show

newtype Graph a b = Graph {
    adjMap  :: Map a [Edge a b]
} deriving Show



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

graph :: Graph [Char] Integer
graph = Graph m

-- get an empty graph
empty :: Graph a b
empty = Graph M.empty


-- get vertices
vertices :: Graph a b -> [a]
vertices = M.keys . adjMap


-- add an edge to the graph
addEdge :: Ord a => Edge a b -> Graph a b -> Graph a b
addEdge e@(Edge src _ _) (Graph m) = Graph (M.insertWith (++) src [e] m)



-- based on key, get edges(value) 
getEdges :: Ord a => a -> Graph a b -> Maybe [Edge a b]
getEdges key (Graph m) = M.lookup key m


-- add a node to the graph
addNode :: Ord a => a -> Graph a b -> Graph a b
addNode node (Graph m) = Graph (M.insert node [] m)


-- get number of edges
nEdges :: Graph a b -> Int
nEdges = undefined


-- returns a list with all nodes adjacent to the given node
neighbours :: Ord a => Graph a b -> a -> [Edge a b]
neighbours (Graph m) key = fromMaybe [] (M.lookup key m)

