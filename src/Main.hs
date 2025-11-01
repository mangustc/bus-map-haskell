module Main where

import KPath (
  findKPathsByLength,
  findKPathsByTransfers,
  )
import Structures
import Text.Read (
  readMaybe,
  )
import Data.List (
  isPrefixOf,
  intercalate,
  concat,
  concatMap,
  find,
  nub,
  intersect,
  sortBy,
  delete,
  sort,
  )
import Data.Maybe (
  mapMaybe, fromJust,
  )
import Data.Array
import qualified Data.Set as Set

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
  let graph = arcsToGraph arcs
  -- print arcs
  -- let paths = findKPathsByLength graph 5 4 6
  -- let paths = findKPathsByLength graph 5 74 134
  -- let paths = findKPathsByLength graph 5 99 133
  let paths = findKPathsByTransfers graph 5 4 6
  -- let paths = findKPathsByTransfers graph 5 74 134
  -- let paths = findKPathsByTransfers graph 5 99 133
  -- mapM_ print (map (\path -> (length path, path)) paths)
  -- print (length paths)

  -- let paths = dfsPathsMat 74 134 mat
  -- let paths = dfsPaths 74 134 arcs
  -- let paths = findPathsByTransfers 4 74 134 mat
  -- let paths = bfsPaths 74 134 arcs
  -- let graph = arcsToGraph arcs
  -- let solution = dijkstra graph 74
  -- let path = pathToNode solution 134
  -- let paths = findKPathsByLength arcs 5 4 6
  -- let paths = findKPathsByTransfers arcs 5 74 134
  -- let paths = findKPathsByLength arcs 5 1 95
  -- let paths = pathsToCurrentPaths arcs (findKPathsByTest arcs 5 4 6)
  -- mapM_ print (map (\(pRIDs, pSIDs) -> (nub pRIDs, pSIDs)) paths)
  -- mapM_ print (map (\(pRIDs, pSIDs) -> (length (nub pRIDs), length pSIDs)) paths)
  -- let paths = findKPathsByTransfers arcs 5 4 6
  -- let paths = findKPathsByLength arcs 5 99 133
  -- let paths = findKPathsByTransfers arcs 5 99 133

  -- putStrLn (intercalate "\n\n" (map show (map (pathToPathSegments arcs) paths)))
  -- print paths
  -- mapM_ print (sortBy (\(node1, _) (node2, _) -> compare node1 node2) solution)
  -- mapM_ print (sortBy (\(_, (_, node1)) (_, (_, node2)) -> compare node1 node2) solution)
  -- mapM_ print (dnodeForNode solution 121)
  -- print path

  -- print (Set.fromList [8,3] `Set.isSubsetOf` Set.fromList [8,3,6])
  -- print ([3,6,2] `elem` map fst [([6,2], []),([3,6,2], [])])
  -- putStrLn (concatMap (const "") paths)
  -- mapM_ print (take 10 arcs)
  -- print (leastTransfers (map arcRoutesIDs (take 10 arcs)))
  -- print (leastTransfers [[27,33],[27],[27,29],[29,11]])
  -- print (leastTransfers [[27], [11]])
  -- mapM_ print paths
  -- mapM_ print paths
  mapM_ print (map (\(pRIDs, pSIDs) -> (nub pRIDs, pSIDs)) paths)
  mapM_ print (map (\(pRIDs, pSIDs) -> (length (nub pRIDs), length pSIDs)) paths)
  -- mapM_ print (map length paths)
  -- mapM_ putStrLn (map (stopIDsToString stops) paths)
  -- print (length paths)
  -- print (map (const "1") paths)
  -- print ([1..1000000000])
  -- mapM_ print (map (\path -> hasDuplicates (map (\arc -> arc.arcStopID) path)) paths)
