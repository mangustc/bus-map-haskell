module Structures where

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
  deriving (Read)
instance Show Stop where
  show (Stop { stopID = sID, stopName = sName}) = sName

getStopByStopID :: [Stop] -> StopID -> Stop
getStopByStopID stops sID = fromMaybe (error "should not be possible") (find (\stop -> stop.stopID == sID) stops)

type RouteID = Int
type RouteName = String
type RoutePath = [StopID]
data Route where
  Route :: {
    routeID :: RouteID,
    routeName :: RouteName,
    routePath :: RoutePath
  } -> Route
  deriving (Read)
instance Show Route where
  show (Route { routeID = rID, routeName = rName, routePath = rPath}) = "Автобус\n\tНазвание: " ++ rName ++ "\n\tПуть: " ++ show rPath

getRouteByRouteID :: [Route] -> RouteID -> Route
getRouteByRouteID routes rID = fromMaybe (error ("should not be possible: " ++ show rID)) (find (\route -> route.routeID == rID) routes)


type PathID = Int
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
type Weight = Int
data QueueElement where
  QueueElement :: {
    qeTransferCost :: Weight,
    qeLengthCost :: Weight,
    qeCurrentStopID :: StopID,
    qePathStopIDs :: [StopID],
    qePathRouteIDs :: [RouteID]
  } -> QueueElement
  deriving (Read, Show)


edgesFromRoutes :: [Route] -> [Edge]
edgesFromRoutes routes = foldr combineEdges []
  (concatMap
    (\r -> zipWith (\ stopID1 stopID2 -> (stopID1, stopID2, [r.routeID])) r.routePath (tail r.routePath))
    routes
  )
  where
    combineEdges :: Edge -> [Edge] -> [Edge]
    combineEdges edge [] = [edge]
    combineEdges edge@(sID, sIDNext, rIDs) (hedge@(hsID, hsIDNext, hrIDs):edges)
      | hsID == sID && hsIDNext == sIDNext = (sID, sIDNext, hrIDs ++ rIDs) : edges
      | otherwise = hedge : combineEdges edge edges

edgesToGraph :: [Edge] -> Graph
edgesToGraph = foldr insertEdge Map.empty

-- Insert an edge into a graph
insertEdge :: Edge -> Graph -> Graph
insertEdge (u,v,w) = Map.insertWith (++) u [(v,w)]

insertSortedByLength :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByLength = insertBy (\qe1 qe2 -> comparing qeLengthCost qe1 qe2 <> comparing qeTransferCost qe1 qe2)

insertSortedByTransfers :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByTransfers = insertBy (\qe1 qe2 -> comparing qeTransferCost qe1 qe2 <> comparing qeLengthCost qe1 qe2)

queueElementToPath :: QueueElement -> Path
queueElementToPath qe = Path {
  pathLength = qe.qeLengthCost,
  pathTransferAmount = qe.qeTransferCost,
  pathFullSegments = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs)),
  pathConciseSegments = getConciseSegments fullSegs
  }
  where
    getConciseSegments :: [PathSegment] -> [PathSegment]
    getConciseSegments [] = []
    getConciseSegments [hps] = [hps]
    getConciseSegments (hps@(hRID, hSID, hSIDNext):tps)
      | nRID == hRID = (hRID, hSID, nSIDNext) : tail next
      | otherwise = hps : next
      where
        next = getConciseSegments tps
        (nRID,nSID,nSIDNext) = head next

    fullSegs = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs))
