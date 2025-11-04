module Structures
(
  StopID(..),
  StopName(..),
  Stop(..),
  RouteID(..),
  RouteName(..),
  Route(..),
  PathSegment(..),
  Path(..),
  Edge(..),
  Graph(..),
  getRouteByRouteID,
  getStopByStopID,
  edgesFromRoutes,
  edgesToGraph,
) where

import qualified Data.Map as Map
import Data.List (insertBy, groupBy, find)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type StopID = Int
type StopName = String
data Stop where
  Stop :: {
    stopID :: StopID,
    stopName :: StopName
  } -> Stop
  deriving (Read, Show)

getStopByStopID :: [Stop] -> StopID -> Stop
getStopByStopID stops sID = fromMaybe (error "should not be possible") (find (\stop -> stop.stopID == sID) stops)

type RouteID = Int
type RouteName = String
data Route where
  Route :: {
    routeID :: RouteID,
    routeName :: RouteName,
    routePath :: [StopID]
  } -> Route
  deriving (Read, Show)

getRouteByRouteID :: [Route] -> RouteID -> Route
getRouteByRouteID routes rID = fromMaybe (error ("should not be possible: " ++ show rID)) (find (\route -> route.routeID == rID) routes)

type PathSegment = (RouteID, StopID, StopID)
data Path where
  Path :: {
    pathLength :: Int,
    pathTransferAmount :: Int,
    pathFullSegments :: [PathSegment],
    pathConciseSegments :: [PathSegment]
  } -> Path
  deriving (Read, Show)

type Edge = (StopID, StopID, [RouteID])
type Graph = Map.Map StopID [(StopID, [RouteID])]


edgesFromRoutes :: [Route] -> [Edge]
edgesFromRoutes routes = foldr combineEdges []
  (concatMap
    (\r -> zipWith (\stopID1 stopID2 -> (stopID1, stopID2, [r.routeID])) r.routePath (tail r.routePath))
    routes
  )
  where
    combineEdges :: Edge -> [Edge] -> [Edge]
    combineEdges edge [] = [edge]
    combineEdges edge@(sID, sIDNext, rIDs) (hedge@(hsID, hsIDNext, hrIDs):edges)
      | hsID == sID && hsIDNext == sIDNext = (sID, sIDNext, hrIDs ++ rIDs) : edges
      | otherwise = hedge : combineEdges edge edges

edgesToGraph :: [Edge] -> Graph
edgesToGraph = foldr (\(sID, sIDNext, rIDs) acc -> Map.insertWith (++) sID [(sIDNext, rIDs)] acc) Map.empty

