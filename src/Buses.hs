{-# LANGUAGE GADTs #-}

import Text.Read (readMaybe)
import Data.List (isPrefixOf, intercalate)
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
  deriving (Show, Read)


getStopByStopID :: [Stop] -> Int -> Stop
getStopByStopID stops sID = head (filter (\s -> stopID s == sID) stops)

getRouteByRouteID :: [Route] -> Int -> Route
getRouteByRouteID routes rID = head (filter (\r -> routeID r == rID) routes)

getRoutePathAsString :: [Stop] -> Route -> String
getRoutePathAsString stops route = "Путь Автобуса " ++ routeName route ++ ":\n"
  ++ intercalate "\n" [show i ++ ". " ++ show (getStopByStopID stops x) | (i, x) <- zip [1..] (routePath route)]


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
  | otherwise = error ("Routes contain non-existent stopIDs: " ++ show [(routeID r, sid) | (r, sid) <- invalidStops])
  where
    invalidStops = [(r, sid) | r <- routes, sid <- routePath r, sid `notElem` map stopID stops]

sanityChecks :: [Stop] -> [Route] -> IO ()
sanityChecks stops routes = do
  _sanityChecks1 stops routes
  _sanityChecks2 stops routes

parseLines :: Read a => [String] -> [a]
parseLines lines = mapMaybe readMaybe filteredLines
  where
    filteredLines = filter (\l -> not (null l || isPrefixOf "--" l)) lines


main :: IO ()
main = do

  stopsFileContents <- readFile "stops.hs"
  let stopsFileLines = lines stopsFileContents 
  let stops = parseLines stopsFileLines :: [Stop]

  routesFileContents <- readFile "routes.hs"
  let routesFileLines = lines routesFileContents 
  let routes = parseLines routesFileLines :: [Route]

  sanityChecks stops routes
  putStrLn (getRoutePathAsString stops (getRouteByRouteID routes 7))

