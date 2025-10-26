{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KPath
(
  findKPathsByLength,
  findKPathsByTransfers,
) where

import Structures
import Data.List (
  sortBy,
  foldl',
  nub,
  sort,
  any,
  concatMap,
  )
import Data.Ord (
  comparing,
  )
import qualified Data.Set as Set


findKPathsByLength :: [Arc] -> Int -> StopID -> StopID -> [[StopID]]
findKPathsByLength arcs pathAmount startStopID endStopID = _findKPathsByLength [] [startStopID] [startStopID]
  where
    _findKPathsByLength :: [[StopID]] -> [StopID] -> [StopID] -> [[StopID]]
    _findKPathsByLength currentPaths visitedStopIDs buildPath@(currentStopID:_)
      | isCurrentPathsComplete && buildPathLength > worstPathLength = currentPaths
      | currentStopID == endStopID =
          let newPaths = reverse buildPath : currentPaths
          in if isCurrentPathsComplete then take pathAmount (sortBy (comparing length) newPaths) else newPaths
      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentStopID && arc.arcStopIDNext `notElem` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> _findKPathsByLength acc (nextArc.arcStopIDNext : visitedStopIDs) (nextArc.arcStopIDNext : buildPath)) currentPaths nextArcs
        where
          buildPathLength = length buildPath
          isCurrentPathsComplete = length currentPaths == pathAmount
          worstPath = if isCurrentPathsComplete then last currentPaths else []
          worstPathLength = length worstPath

appendNext :: [RouteID] -> RouteID -> [RouteID]
appendNext [] x = [x]
appendNext xs x
  | last xs == x = xs
  | otherwise    = xs ++ [x]

findKPathsByTransfers :: [Arc] -> Int -> StopID -> StopID -> [([StopID], [RouteID])]
findKPathsByTransfers arcs pathAmount startStopID endStopID = _findKPathsByTransfers [] [startStopID] ([], [startStopID])
  where
    _findKPathsByTransfers :: [([RouteID], [StopID])] -> [StopID] -> ([RouteID], [StopID]) -> [([RouteID], [StopID])]
    _findKPathsByTransfers currentPaths visitedStopIDs (buildPathRouteIDs, buildPath)
      | any (\(pRIDs, pSIDs) -> pRIDs == buildPathRouteIDs && length pSIDs <= buildPathLength) currentPaths = currentPaths
      | isCurrentPathsComplete && (buildPathRouteIDsLength > worstPathRouteIDsLength || (buildPathRouteIDsLength == worstPathRouteIDsLength) && (buildPathLength > worstPathLength)) = currentPaths
      | currentStopID == endStopID =
          -- let newPaths = nubBy (\p1 p2 -> fst p1 == fst p2) ((reverse buildPathRouteIDs, reverse buildPath) : currentPaths)
          let newPaths = ((buildPathRouteIDs, reverse buildPath) : currentPaths)
          in if isCurrentPathsComplete then take pathAmount (sortBy (comparing (length . fst) <> comparing (length . snd)) newPaths) else newPaths
      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentStopID && arc.arcStopIDNext `notElem` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl (\acc2 nextRID -> _findKPathsByTransfers acc2 (nextArc.arcStopIDNext : visitedStopIDs) (appendNext buildPathRouteIDs nextRID, nextArc.arcStopIDNext : buildPath)) acc nextArc.arcRoutesIDs) currentPaths nextArcs
        where
          buildPathRouteIDsLength = length buildPathRouteIDs
          buildPathLength = length buildPath
          currentStopID = head buildPath

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRouteIDs, worstPath) = if isCurrentPathsComplete then last currentPaths else ([],[])
          worstPathRouteIDsLength = length worstPathRouteIDs
          worstPathLength = length worstPath
