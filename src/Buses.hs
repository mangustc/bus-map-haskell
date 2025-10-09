{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Text.Read (readMaybe)
import Data.List (isPrefixOf, intercalate, concat, concatMap)
import Data.Maybe (mapMaybe)

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
type PathSegments = [PathSegment]
data Path where
  Path :: {
    pathID :: PathID,
    pathSegments :: PathSegments
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


getStopByStopID :: [Stop] -> Int -> Stop
getStopByStopID stops sID = head (filter (\s -> s.stopID == sID) stops)

getRouteByRouteID :: [Route] -> Int -> Route
getRouteByRouteID routes rID = head (filter (\r -> r.routeID == rID) routes)

getRoutePathAsString :: [Stop] -> Route -> String
getRoutePathAsString stops route = "Путь Автобуса " ++ route.routeName ++ ":\n"
  ++ intercalate "\n" [show i ++ ". " ++ show (getStopByStopID stops x) | (i, x) <- zip [1..] (route.routePath)]


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

_sanityChecks1 :: [Stop] -> [Route] -> IO ()
_sanityChecks1 stops routes
  | hasDuplicates (map stopID stops) || hasDuplicates (map routeID routes)
    = error "Stops have duplicate StopID or Routes have duplicate RouteID"
  | otherwise = return ()

_sanityChecks2 :: [Stop] -> [Route] -> IO ()
_sanityChecks2 stops routes
  | null invalidStops = return ()
  | otherwise = error ("Routes contain non-existent stopIDs: " ++ show [(r.routeID, sid) | (r, sid) <- invalidStops])
  where
    invalidStops = [(r, sid) | r <- routes, sid <- r.routePath, sid `notElem` map stopID stops]

sanityChecks :: [Stop] -> [Route] -> IO ()
sanityChecks stops routes = do
  _sanityChecks1 stops routes
  _sanityChecks2 stops routes

parseLines :: Read a => [String] -> [a]
parseLines lines = mapMaybe readMaybe filteredLines
  where
    filteredLines = filter (\l -> not (null l || isPrefixOf "--" l)) lines

_combineArcs :: Arc -> [Arc] -> [Arc]
_combineArcs arc [] = [arc]
_combineArcs arc (harc:arcs)
  | harc.arcStopID == arc.arcStopID && harc.arcStopIDNext == arc.arcStopIDNext = Arc {arcStopID = arc.arcStopID, arcStopIDNext = arc.arcStopIDNext, arcRoutesIDs = harc.arcRoutesIDs ++ arc.arcRoutesIDs} : arcs
  | otherwise = harc : _combineArcs arc arcs

arcsFromRoutes :: [Route] -> [Arc]
arcsFromRoutes routes = foldr _combineArcs []
  (concatMap
    (\r -> zipWith (\ stopID1 stopID2 -> Arc {arcStopID = stopID1, arcStopIDNext = stopID2, arcRoutesIDs = [r.routeID]}) r.routePath (tail r.routePath))
    routes
  )

main :: IO ()
main = do

  stopsFileContents <- readFile "stops.hs"
  let stopsFileLines = lines stopsFileContents
  let stops = parseLines stopsFileLines :: [Stop]

  routesFileContents <- readFile "routes.hs"
  let routesFileLines = lines routesFileContents
  let routes = parseLines routesFileLines :: [Route]

  sanityChecks stops routes

  let arcs = arcsFromRoutes routes
  print arcs

