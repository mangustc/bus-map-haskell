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
  edgesFromRoutesStops,
  edgesToGraph,
) where

import qualified Data.Map as Map
import Data.List (insertBy, groupBy, find, isInfixOf, nub)
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


edgesFromRoutesStops :: [Route] -> [Stop] -> [Edge]
edgesFromRoutesStops routes stops = foldr combineEdges fromRoutes (
    map (\(sID, sIDNext, _) -> (sIDNext, sID, [0])) fromRoutesNoPair
    ++ map (\(sID, sIDNext, _) -> (sID, sIDNext, [0])) fromRoutesNoPair
    ++ concatMap (\(s1, s2) -> [(s1.stopID, s2.stopID, [0]), (s2.stopID, s1.stopID, [0])]) pairs
    )
  where
    combineEdges :: Edge -> [Edge] -> [Edge]
    combineEdges edge [] = [edge]
    combineEdges edge@(sID, sIDNext, rIDs) (hedge@(hsID, hsIDNext, hrIDs):edges)
      | hsID == sID && hsIDNext == sIDNext = (sID, sIDNext, nub (hrIDs ++ rIDs)) : edges
      | otherwise = hedge : combineEdges edge edges

    fromRoutes = foldr combineEdges [] (concatMap (\r -> zipWith (\stopID1 stopID2 -> (stopID1, stopID2, [r.routeID])) r.routePath (tail r.routePath)) routes)
    fromRoutesNoPair = filter (\(sID, sIDNext, rIDs) -> (sIDNext, sID, rIDs) `notElem` fromRoutes) fromRoutes

    stopsPaired = filter (\stop -> '(' `elem` stop.stopName) stops
    stopsNorth = filter (\stop -> "Северная" `isInfixOf` stop.stopName) stopsPaired
    stopsSouth = filter (\stop -> "Южная" `isInfixOf` stop.stopName) stopsPaired
    stopsWest = filter (\stop -> "Западная" `isInfixOf` stop.stopName) stopsPaired
    stopsEast = filter (\stop -> "Восточная" `isInfixOf` stop.stopName) stopsPaired
    pairsNorthSouth = filter (\(n, s) -> let (nName, nDir) = break (== '(') n.stopName
                                             (sName, sDir) = break (== '(') s.stopName
                                         in (nName == sName) && (drop 8 nDir == drop 5 sDir)) [(sn, ss) | sn <- stopsNorth, ss <- stopsSouth]
    pairsWestEast = filter (\(w, e) -> let (wName, wDir) = break (== '(') w.stopName
                                           (eName, eDir) = break (== '(') e.stopName
                                       in (wName == eName) && (drop 8 wDir == drop 5 eDir)) [(sw, se) | sw <- stopsWest, se <- stopsEast]
    pairs = pairsNorthSouth ++ pairsWestEast

edgesToGraph :: [Edge] -> Graph
edgesToGraph = foldr (\(sID, sIDNext, rIDs) acc -> Map.insertWith (++) sID [(sIDNext, rIDs)] acc) Map.empty

