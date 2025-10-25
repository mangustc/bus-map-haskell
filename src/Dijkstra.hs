module Dijkstra 
(
  dijkstra,
  pathToNode,
  edgesFor,
  dnodeForNode,
  arcsToGraph,
  Edge(..),
  Node,
  Graph,
  Dnode,
) where

import Structures
import Data.List

data Edge = Edge { node::Node, weight::Float } deriving (Show)
type Node = StopID
type Graph = [(Node, [Edge])]
type Dnode = (Node, (Float, Node))


appendReversed :: [((Node, Node), Float)] -> [((Node, Node), Float)]
appendReversed es = es ++ map (\((n1,n2),w) -> ((n2,n1),w)) es

-- Given a weighted graph and a node, return the edges incident on the node
edgesFor :: Graph -> Node -> [Edge]
edgesFor g n = snd (head (filter (\(nd, _) -> nd == n) g))

-- Given a node and a list of edges, one of which is incident on the node, return the weight
weightFor :: Node -> [Edge] -> Float
weightFor n edges = weight (head (filter (\e -> n == node e) edges))

-- Given a list of edges, return their nodes
connectedNodes :: [Edge] -> [Node]
connectedNodes edges = map node edges

dnodeForNode :: [Dnode] -> Node -> Dnode
dnodeForNode dnodes node = head (filter (\(x, _) -> x == node) dnodes)

-- Given a graph and a start node
dijkstra :: Graph -> Node -> [Dnode]
dijkstra g start =
  let dnodes = initD g start
      unchecked = map fst dnodes
  in  dijkstra' g dnodes unchecked

-- Given a graph and a start node, construct an initial list of Dnodes
initD :: Graph -> Node -> [Dnode]
initD g start =
  let initDist (n, es)
        | n == start = 0
        | start `elem` connectedNodes es = weightFor start es
        | otherwise = 1.0/0.0
  in map (\pr@(n, _) -> (n, (initDist pr, start))) g

-- Dijkstra's algorithm (recursive)
-- get a list of Dnodes that haven't been checked yet
-- select the one with minimal distance and add it to the checked list. Call it current.
-- update each Dnode that connects to current by comparing 
-- the Dnode's current distance to the sum: (weight of the connecting edge + current's distance)
-- the algorithm terminates when all nodes have been checked.
dijkstra' :: Graph -> [Dnode] -> [Node] -> [Dnode]
dijkstra' g dnodes [] = dnodes
dijkstra' g dnodes unchecked =
  let dunchecked = filter (\dn -> fst dn `elem` unchecked) dnodes
      current = head (sortBy (\(_,(d1,_)) (_,(d2,_)) -> compare d1 d2) dunchecked)
      c = fst current
      unchecked' = delete c unchecked
      edges = edgesFor g c
      cnodes = intersect (connectedNodes edges) unchecked'
      dnodes' = map (\dn -> update dn current cnodes edges) dnodes
  in dijkstra' g dnodes' unchecked'

-- given a Dnode to update, the current Dnode, the Nodes connected to current 
-- and current's edges, return a (possibly) updated Dnode
update :: Dnode -> Dnode -> [Node] -> [Edge] -> Dnode
update dn@(n, (nd, p)) (c, (cd, _)) cnodes edges =
  let wt = weightFor n edges
  in  if n `notElem` cnodes then dn
      else if cd+wt < nd then (n, (cd+wt, c)) else dn


-- Идет обратно от цели до начала (решения)
-- given a Dijkstra solution and a destination node, return the path to it.
pathToNode :: [Dnode] -> Node -> [Node]
pathToNode dnodes dest =
  let dn@(n, (d, p)) = dnodeForNode dnodes dest
  in if n == p then [n] else pathToNode dnodes p ++ [n]

arcsToGraph :: [Arc] -> Graph
arcsToGraph arcs = foldr insertArc [] arcs
  where
    insertArc (Arc sID sIDNext rIDs) graph =
      let w = if null rIDs then 0 else 1
          newEdge = Edge sIDNext w
          -- Insert or update the node's adjacency list
          update [] = [(sID, [newEdge])]
          update ((n, edges) : rest)
            | n == sID = (n, newEdge : edges) : rest
            | otherwise = (n, edges) : update rest
      in update graph

