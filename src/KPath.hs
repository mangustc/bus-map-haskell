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
appendNext rIDs x
  | head rIDs == x = rIDs
  | otherwise = x : rIDs

findKPathsByTransfers :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByTransfers arcs pathAmount startStopID endStopID = map (\(rIDs, sIDs) -> (reverse rIDs, sIDs)) (_findKPathsByTransfers [] (Set.singleton startStopID) ([], [startStopID]))
  where
    maximumRouteIDs = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByTransfers :: [([RouteID], [StopID])] -> Set.Set StopID -> ([RouteID], [StopID]) -> [([RouteID], [StopID])]
    _findKPathsByTransfers currentPaths visitedStopIDs (buildPathRouteIDs, buildPath)
      | buildPathRouteIDsLength > maximumRouteIDs = currentPaths
      | any (\(pRIDs, pSIDs) -> pRIDs == buildPathRouteIDs && length pSIDs <= buildPathLength) currentPaths = currentPaths
      | isCurrentPathsComplete && (buildPathRouteIDsLength > worstPathRouteIDsLength || (buildPathRouteIDsLength == worstPathRouteIDsLength) && (buildPathLength > worstPathLength)) = currentPaths
      | currentStopID == endStopID =
          let newPaths = ((buildPathRouteIDs, reverse buildPath) : currentPaths)
          in if isCurrentPathsComplete then take pathAmount (sortBy (comparing (length . fst) <> comparing (length . snd)) newPaths) else newPaths
      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentStopID && Set.notMember arc.arcStopIDNext visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            let newVisited = Set.insert nextArc.arcStopIDNext visitedStopIDs
                newBuildPathRouteIDs = appendNext buildPathRouteIDs nextRID
            in _findKPathsByTransfers acc2 newVisited (newBuildPathRouteIDs, nextArc.arcStopIDNext : buildPath)) acc nextArc.arcRoutesIDs) currentPaths nextArcs
        where
          buildPathRouteIDsLength = length buildPathRouteIDs
          buildPathLength = length buildPath
          currentStopID = head buildPath

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRouteIDs, worstPath) = if isCurrentPathsComplete then last currentPaths else ([],[])
          worstPathRouteIDsLength = length worstPathRouteIDs
          worstPathLength = length worstPath
