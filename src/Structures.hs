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
getRouteByRouteID routes rID = fromMaybe (error "should not be possible") (find (\route -> route.routeID == rID) routes)


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

data Arc where
  Arc :: {
    arcStopID :: StopID,
    arcStopIDNext :: StopID,
    arcRoutesIDs :: [RouteID]
  } -> Arc
  deriving (Read, Ord)
instance Show Arc where
  show (Arc { arcStopID = sID1, arcStopIDNext = sID2, arcRoutesIDs = rIDs}) = show sID1 ++ "-" ++ show rIDs ++ "->" ++ show sID2
instance Eq Arc where
  Arc { arcStopID = sID1, arcStopIDNext = sIDNext1, arcRoutesIDs = rIDs1} == Arc { arcStopID = sID2, arcStopIDNext = sIDNext2, arcRoutesIDs = rIDs2} = sID1 == sID2 && sIDNext1 == sIDNext2

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


arcsFromRoutes :: [Route] -> [Arc]
arcsFromRoutes routes = foldr combineArcs []
  (concatMap
    (\r -> zipWith (\ stopID1 stopID2 -> Arc {arcStopID = stopID1, arcStopIDNext = stopID2, arcRoutesIDs = [r.routeID]}) r.routePath (tail r.routePath))
    routes
  )
  where
    combineArcs :: Arc -> [Arc] -> [Arc]
    combineArcs arc [] = [arc]
    combineArcs arc (harc:arcs)
      | harc.arcStopID == arc.arcStopID && harc.arcStopIDNext == arc.arcStopIDNext = Arc {arcStopID = arc.arcStopID, arcStopIDNext = arc.arcStopIDNext, arcRoutesIDs = harc.arcRoutesIDs ++ arc.arcRoutesIDs} : arcs
      | otherwise = harc : combineArcs arc arcs

arcsToGraph :: [Arc] -> Graph
arcsToGraph arcs = foldr (insertEdge . (\arc -> (arc.arcStopID, arc.arcStopIDNext, arc.arcRoutesIDs))) Map.empty arcs

-- Insert an edge into a graph
insertEdge :: Edge -> Graph -> Graph
insertEdge (u,v,w) graph  = Map.insertWith (++) u [(v,w)] graph

insertSortedByLength :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByLength newEntry queue = insertBy (\qe1 qe2 -> comparing qeLengthCost qe1 qe2 <> comparing qeTransferCost qe1 qe2) newEntry queue

insertSortedByTransfers :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByTransfers newEntry queue = insertBy (\qe1 qe2 -> comparing qeTransferCost qe1 qe2 <> comparing qeLengthCost qe1 qe2) newEntry queue

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
