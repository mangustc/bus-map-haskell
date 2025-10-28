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
  any,
  concatMap,
  intersect,
  )
import Data.Ord (
  comparing,
  )
import qualified Data.Set as Set


appendNext :: [RouteID] -> RouteID -> [RouteID]
appendNext [] x = [x]
appendNext rIDs x
  | head rIDs == x = rIDs
  | otherwise = x : rIDs

isPointlessAgainst :: ([RouteID], [StopID]) -> ([RouteID], [StopID]) -> Bool
isPointlessAgainst (pointlessRIDs, pointlessSIDs) (betterRIDs, betterSIDs) =
  let pointlessRIDsLength = length pointlessRIDs
      pointlessSIDsLength = length pointlessSIDs
      betterRIDsLength = length betterRIDs
      betterSIDsLength = length betterSIDs

      betterRIDsInPointlessRIDs = pointlessRIDs `intersect` betterRIDs
      betterRIDsInPointlessRIDsLength = length betterRIDsInPointlessRIDs
      betterSIDsInPointlessSIDs = pointlessSIDs `intersect` betterSIDs
      betterSIDsInPointlessSIDsLength = length betterSIDsInPointlessSIDs
  in (betterSIDsInPointlessSIDsLength == betterSIDsLength && betterRIDsInPointlessRIDsLength == betterRIDsLength)

findKPathsByTransfers :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByTransfers arcs pathAmount startStopID endStopID = map (\(rIDs, sIDs) -> (reverse rIDs, reverse sIDs)) (_findKPathsByTransfers [] (Set.singleton startStopID) ([], [startStopID]))
  where
    maximumRouteIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByTransfers :: [([RouteID], [StopID])] -> Set.Set StopID -> ([RouteID], [StopID]) -> [([RouteID], [StopID])]
    _findKPathsByTransfers currentPaths visitedStopIDs (buildPathRouteIDs, buildPath)
      -- Reasons to end search early
      | buildPathRouteIDsLength > maximumRouteIDsAmount = currentPaths
      | Set.size (Set.fromList buildPathRouteIDs) /= buildPathRouteIDsLength = currentPaths
      | any (\(pRIDs, pSIDs) -> pRIDs == buildPathRouteIDs && length pSIDs <= buildPathLength) currentPaths = currentPaths
      | isCurrentPathsComplete && (buildPathRouteIDsLength > worstPathRouteIDsLength || (buildPathRouteIDsLength == worstPathRouteIDsLength) && (buildPathLength > worstPathLength)) = currentPaths

      -- has come to the end
      | currentStopID == endStopID =
          let newPath = (buildPathRouteIDs, buildPath)
              shouldIncludePath = not (any (\path -> newPath `isPointlessAgainst` path) currentPaths)
              filteredPaths = filter (\path -> not (path `isPointlessAgainst` newPath)) currentPaths
              newPaths = sortBy (comparing (length . fst) <> comparing (length . snd)) (if shouldIncludePath then newPath : filteredPaths else filteredPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths
          -- let newPaths = ((buildPathRouteIDs, reverse buildPath) : currentPaths)
          -- in if isCurrentPathsComplete then take pathAmount (sortBy (comparing (length . snd) <> comparing (length . fst)) newPaths) else newPaths

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

findKPathsByLength :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByLength arcs pathAmount startStopID endStopID = map (\(rIDs, sIDs) -> (reverse rIDs, reverse sIDs)) (_findKPathsByLength [] (Set.singleton startStopID) ([], [startStopID]))
  where
    maximumRouteIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByLength :: [([RouteID], [StopID])] -> Set.Set StopID -> ([RouteID], [StopID]) -> [([RouteID], [StopID])]
    _findKPathsByLength currentPaths visitedStopIDs (buildPathRouteIDs, buildPath)
      -- Reasons to end search early
      | buildPathRouteIDsLength > maximumRouteIDsAmount = currentPaths
      | Set.size (Set.fromList buildPathRouteIDs) /= buildPathRouteIDsLength = currentPaths
      | any (\(pRIDs, pSIDs) -> pRIDs == buildPathRouteIDs && length pSIDs <= buildPathLength) currentPaths = currentPaths
      | isCurrentPathsComplete && (buildPathLength > worstPathLength || (buildPathLength == worstPathLength) && (buildPathRouteIDsLength > worstPathRouteIDsLength)) = currentPaths

      -- has come to the end
      | currentStopID == endStopID =
          let newPath = (buildPathRouteIDs, buildPath)
              shouldIncludePath = not (any (\path -> newPath `isPointlessAgainst` path) currentPaths)
              filteredPaths = filter (\path -> not (path `isPointlessAgainst` newPath)) currentPaths
              newPaths = sortBy (comparing (length . snd) <> comparing (length . fst)) (if shouldIncludePath then newPath : filteredPaths else filteredPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths
          -- let newPaths = ((buildPathRouteIDs, reverse buildPath) : currentPaths)
          -- in if isCurrentPathsComplete then take pathAmount (sortBy (comparing (length . snd) <> comparing (length . fst)) newPaths) else newPaths

      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentStopID && Set.notMember arc.arcStopIDNext visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            let newVisited = Set.insert nextArc.arcStopIDNext visitedStopIDs
                newBuildPathRouteIDs = appendNext buildPathRouteIDs nextRID
            in _findKPathsByLength acc2 newVisited (newBuildPathRouteIDs, nextArc.arcStopIDNext : buildPath)) acc nextArc.arcRoutesIDs) currentPaths nextArcs
        where
          buildPathRouteIDsLength = length buildPathRouteIDs
          buildPathLength = length buildPath
          currentStopID = head buildPath

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRouteIDs, worstPath) = if isCurrentPathsComplete then last currentPaths else ([],[])
          worstPathRouteIDsLength = length worstPathRouteIDs
          worstPathLength = length worstPath
