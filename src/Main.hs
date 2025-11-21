module Main where

import KPath (
  findEveryKPath,
  getKPathsByLength,
  getKPathsByTransfers,
  QueueElement,
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
import CLI (
  cliProcess
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

  cliProcess stops routes

  -- let edges = edgesFromRoutesStops routes stops
  -- let graph = edgesToGraph edges
  -- let paths = findEveryKPath graph 5 99 133
  -- -- mapM_ print (findKPathsByLength graph 5 99 302)
  -- mapM_ print (getKPathsByTransfers paths)
  -- -- mapM_ print (findKPathsByLength graph 5 99 133)
