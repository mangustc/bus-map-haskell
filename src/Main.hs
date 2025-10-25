module Main where

import Dijkstra
import KPath (findKPathsByLength, findKPathsByTransfers)
import Structures
import Text.Read (readMaybe)
import Data.List (isPrefixOf, intercalate, concat, concatMap, find, nub, intersect, sortBy, delete)
import Data.Maybe (mapMaybe)
import Data.Array


getStopByStopID :: [Stop] -> Int -> Stop
getStopByStopID stops sID = head (filter (\s -> s.stopID == sID) stops)

getRouteByRouteID :: [Route] -> Int -> Route
getRouteByRouteID routes rID = head (filter (\r -> r.routeID == rID) routes)

getRoutePathAsString :: [Stop] -> Route -> String
getRoutePathAsString stops route = "Путь Автобуса " ++ route.routeName ++ ":\n"
  ++ intercalate "\n" [show i ++ ". " ++ show (getStopByStopID stops x) | (i, x) <- zip [1..] route.routePath]


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



arcsToAdjMatrix :: [Arc] -> Adjmat
arcsToAdjMatrix arcs = array bounds assocList
  where
    -- Extract all stop IDs
    stops = nub $ concat [[arcStopID a, arcStopIDNext a] | a <- arcs]
    -- Define the bounds for the matrix (minStopID, minStopID) to (maxStopID, maxStopID)
    minStop = minimum stops
    maxStop = maximum stops
    bounds = ((minStop, minStop), (maxStop, maxStop))

    -- Create an association list ((source,dest), routeIDs)
    -- For pairs missing in arcs, default to []
    assocList = [((i, j), findRoutes i j) | i <- [minStop .. maxStop], j <- [minStop .. maxStop]]

    -- Find route IDs for given i -> j or []
    findRoutes i j = case filter (\a -> arcStopID a == i && arcStopIDNext a == j) arcs of
      (Arc _ _ rIDs : _) -> rIDs
      [] -> []

stopIDsToString :: [Stop] -> [StopID] -> String
stopIDsToString stops stopIDs = "Путь:\n" ++ intercalate "\n" (map (\sID -> (getStopByStopID stops sID).stopName) stopIDs) ++ "\n"

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
  let mat = arcsToAdjMatrix arcs
  -- print arcs

  -- let paths = dfsPathsMat 74 134 mat
  -- let paths = dfsPaths 74 134 arcs
  -- let paths = findPathsByTransfers 4 74 134 mat
  -- let paths = bfsPaths 74 134 arcs
  -- let graph = arcsToGraph arcs
  -- let solution = dijkstra graph 74
  -- let path = pathToNode solution 134
  -- let paths = findKPathsByLength arcs 500 74 134
  let paths = findKPathsByTransfers arcs 6 74 134

  -- mapM_ print (sortBy (\(node1, _) (node2, _) -> compare node1 node2) solution)
  -- mapM_ print (sortBy (\(_, (_, node1)) (_, (_, node2)) -> compare node1 node2) solution)
  -- mapM_ print (dnodeForNode solution 121)
  -- print path

  -- print ([3,6,2] `elem` map fst [([6,2], []),([3,6,2], [])])
  -- putStrLn (concatMap (const "") paths)
  -- mapM_ print (take 10 arcs)
  -- print (leastTransfers (map arcRoutesIDs (take 10 arcs)))
  -- print (leastTransfers [[27,33],[27],[27,29],[29,11]])
  -- print (leastTransfers [[27], [11]])
  mapM_ print paths
  -- mapM_ print (map length paths)
  -- mapM_ putStrLn (map (stopIDsToString stops) paths)
  -- print (length paths)
  -- print (map (const "1") paths)
  -- print ([1..1000000000])
  -- mapM_ print (map (\path -> hasDuplicates (map (\arc -> arc.arcStopID) path)) paths)

--
--
-- import Control.Lens
-- import Data.Maybe
-- import Data.Text (Text)
-- import Monomer
-- import TextShow
--
-- import qualified Monomer.Lens as L
--
-- newtype AppModel = AppModel {
--   _clickCount :: Int
-- } deriving (Eq, Show)
--
-- data AppEvent
--   = AppInit
--   | AppIncrease
--   deriving (Eq, Show)
--
-- makeLenses 'AppModel
--
-- buildUI
--   :: WidgetEnv AppModel AppEvent
--   -> AppModel
--   -> WidgetNode AppModel AppEvent
-- buildUI wenv model = widgetTree where
--   widgetTree = vstack [
--       label "Hello world",
--       spacer,
--       hstack [
--         label $ "Click count: " <> showt (model ^. clickCount),
--         spacer,
--         button "Increase count" AppIncrease
--       ]
--     ] `styleBasic` [padding 10]
--
-- handleEvent
--   :: WidgetEnv AppModel AppEvent
--   -> WidgetNode AppModel AppEvent
--   -> AppModel
--   -> AppEvent
--   -> [AppEventResponse AppModel AppEvent]
-- handleEvent wenv node model evt = case evt of
--   AppInit -> []
--   AppIncrease -> [Model (model & clickCount +~ 1)]
--
-- main :: IO ()
-- main = do
--   startApp model handleEvent buildUI config
--   where
--     config = [
--       appWindowTitle "Hello world",
--       appWindowIcon "./assets/images/icon.png",
--       appTheme lightTheme,
--       appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
--       appInitEvent AppInit
--       ]
--     model = AppModel 0
