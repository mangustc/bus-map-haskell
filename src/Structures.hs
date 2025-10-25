module Structures where

import Data.Array

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

type PathID = Int
type PathSegment = (RouteID, StopID, StopID)
data Path where
  Path :: {
    pathID :: PathID,
    pathSegments :: [PathSegment]
  } -> Path
  deriving (Show, Read)

data Arc where
  Arc :: {
    arcStopID :: StopID,
    arcStopIDNext :: StopID,
    arcRoutesIDs :: [RouteID]
  } -> Arc
  deriving (Read)
instance Show Arc where
  show (Arc { arcStopID = sID1, arcStopIDNext = sID2, arcRoutesIDs = rIDs}) = show sID1 ++ "-" ++ show rIDs ++ "->" ++ show sID2
instance Eq Arc where
  Arc { arcStopID = sID1, arcStopIDNext = sIDNext1, arcRoutesIDs = rIDs1} == Arc { arcStopID = sID2, arcStopIDNext = sIDNext2, arcRoutesIDs = rIDs2} = sID1 == sID2 && sIDNext1 == sIDNext2

type Adjmat = Array (StopID, StopID) [RouteID]
