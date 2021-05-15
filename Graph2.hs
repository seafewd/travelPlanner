-- Implement a module in Haskell that exports the type Graph and corresponds to thegraph implementation of question 1.
-- The module should export the following functions:
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

-- This is a clever trick to implement
-- transposition in constant time.
data Graph t = G { map :: M.Map t (S.Set t), transposeMap :: M.Map t (S.Set t) }

-- creates an empty graph
empty :: Graph t 
empty = G M.empty M.empty

-- adds a node with given label toa given graph and returns the resulting graph
addNode :: Ord t => t -> Graph t -> Graph t 
addNode t g@(G m tm)  
    | t `M.member` m = g  
    | otherwise      = G (M.insert t S.empty m) (M.insert t S.empty tm)
    
-- adds an edge from the first node to the second provided that the two nodes exist
addEdge :: Ord t => t -> t -> Graph t -> Graph t 
addEdge src dest g@(G m tm)  
    | src `M.member` m && dest `M.member` m = G (M.update (Just . S.insert dest) src m) (M.update (Just . S.insert src) dest tm) 
    | otherwise = g
    
--which returns the number of nodes
nNodes :: Graph t -> Int
nNodes = length . nodes


-- returns the number of edges
nEdges :: Graph t -> Int
nEdges (G m _) = foldr (\s n -> S.size s + n) 0 m


-- returns a list with all nodes
nodes :: Graph t -> [t]
nodes (G m _) = M.keys m


-- returns a list with all nodes adjecent to the given node
neighbours :: Ord t => Graph t -> t -> [t] 
neighbours (G m _) t = maybe [] S.toList (M.lookup t m)


-- Add, to either the Java (question 1) or Haskell (question 2) implemention, a
-- method/function isCyclic which determines whether the graph/a given graph is
-- cyclic (contains a cycle) and returns a boolean value.

-- First a helper to remomve nodes
removeNode :: Ord t => t -> Graph t -> Graph t
removeNode n (G m tm) = 
    let m' = M.delete n m
        m'' = M.map (S.delete n) m'
        tm' = M.delete n tm
        tm'' = M.map (S.delete n) tm'
    in G m'' tm''
    
-- Key idea: if the graph is acyclic then it has a topsort. All we need to do is try to topsort
-- the graph, and if we ever reach a state where there are no indegree 0 nodes, we are done.
-- The nice thing is that because we keep track of the transposed graph, findign the indegree
-- of a node is straightforward, just check the outdegree in the transposed graph.

isCyclic :: Ord t => Graph t -> Bool
isCyclic g@(G m tm)  
    | nNodes g == 0 = True  
    | otherwise     =    
        -- Find all the nodes with indegree zero (i.e. the ones with outdegree zero in the transposed graph)    
        let indegreeZeroNodes = M.keys (M.filter S.null tm) in
        if null emptyKeys
            then False 
            else isCyclic (foldr removeNode g indegreeZeroNodes)
        
    --Add a method to the class in question 1 or a function to the module in question 2 that transposes the graph/a given graph.
    
transpose :: Graph t -> Graph t
transpose (G m tm) = G tm m