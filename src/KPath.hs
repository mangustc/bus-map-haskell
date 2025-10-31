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

isPointlessAgainst :: (Set.Set RouteID, [StopID], [RouteID]) -> (Set.Set RouteID, [StopID], [RouteID]) -> Bool
isPointlessAgainst (pointlessRIDs, pointlessSIDs, _) (betterRIDs, betterSIDs, _) =
  let betterRIDsLength = Set.size betterRIDs
      betterSIDsLength = length betterSIDs
      pointlessSIDsLength = length pointlessSIDs
  in (betterRIDs `Set.isSubsetOf` pointlessRIDs && pointlessSIDsLength >= betterSIDsLength)

findKPathsByTransfers :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByTransfers arcs pathAmount startSID endSID = map (\(_, sIDs, rIDs) -> (tail (reverse rIDs), reverse sIDs)) result
  where
    maximumRIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByTransfers :: [(Set.Set RouteID, [StopID], [RouteID])] -> Set.Set StopID -> (Set.Set RouteID, [StopID], [RouteID]) -> [(Set.Set RouteID, [StopID], [RouteID])]
    _findKPathsByTransfers currentPaths visitedStopIDs buildPath@(buildPathRIDs, buildPathSIDs, buildPathRIDsPath)
      -- Reasons to end search early
      | buildPathRIDsLength > maximumRIDsAmount = currentPaths
      | isCurrentPathsComplete && (buildPathRIDsLength > worstPathRIDsLength || (buildPathRIDsLength == worstPathRIDsLength) && (buildPathSIDsLength > worstPathSIDsLength)) = currentPaths
      | any (\path -> buildPath `isPointlessAgainst` path) currentPaths = currentPaths

      -- has come to the end
      | currentSID == endSID =
          let newPaths = sortBy (\(pRIDs1, pSIDs1, _) (pRIDs2, pSIDs2, _) -> comparing Set.size pRIDs1 pRIDs2 <> comparing length pSIDs1 pSIDs2) (buildPath : filter (\path -> not (path `isPointlessAgainst` buildPath)) currentPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths

      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentSID && arc.arcStopIDNext `Set.notMember` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            (_findKPathsByTransfers acc2 (Set.insert nextArc.arcStopIDNext visitedStopIDs) (Set.insert nextRID buildPathRIDs, nextArc.arcStopIDNext : buildPathSIDs, nextRID : buildPathRIDsPath))
            ) acc (filter (\rID -> head buildPathRIDsPath == rID || rID `Set.notMember` buildPathRIDs) nextArc.arcRoutesIDs)) currentPaths nextArcs
        where
          buildPathRIDsLength = Set.size buildPathRIDs
          buildPathSIDsLength = length buildPathSIDs
          currentSID = head buildPathSIDs

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRIDs, worstPathSIDs, _) = if isCurrentPathsComplete then last currentPaths else (Set.empty,[],[])
          worstPathRIDsLength = Set.size worstPathRIDs
          worstPathSIDsLength = length worstPathSIDs
    result = _findKPathsByTransfers [] (Set.singleton startSID) (Set.empty, [startSID], [0])

findKPathsByLength :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByLength arcs pathAmount startSID endSID = map (\(_, sIDs, rIDs) -> (tail (reverse rIDs), reverse sIDs)) result
  where
    maximumRIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByLength :: [(Set.Set RouteID, [StopID], [RouteID])] -> Set.Set StopID -> (Set.Set RouteID, [StopID], [RouteID]) -> [(Set.Set RouteID, [StopID], [RouteID])]
    _findKPathsByLength currentPaths visitedStopIDs buildPath@(buildPathRIDs, buildPathSIDs, buildPathRIDsPath)
      -- Reasons to end search early
      | buildPathRIDsLength > maximumRIDsAmount = currentPaths
      | isCurrentPathsComplete && (buildPathSIDsLength > worstPathSIDsLength || (buildPathSIDsLength == worstPathSIDsLength) && (buildPathRIDsLength > worstPathRIDsLength)) = currentPaths
      | any (\path -> buildPath `isPointlessAgainst` path) currentPaths = currentPaths

      -- has come to the end
      | currentSID == endSID =
          let newPaths = sortBy (\(pRIDs1, pSIDs1, _) (pRIDs2, pSIDs2, _) -> comparing length pSIDs1 pSIDs2 <> comparing Set.size pRIDs1 pRIDs2) (buildPath : filter (\path -> not (path `isPointlessAgainst` buildPath)) currentPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths

      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentSID && arc.arcStopIDNext `Set.notMember` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            (_findKPathsByLength acc2 (Set.insert nextArc.arcStopIDNext visitedStopIDs) (Set.insert nextRID buildPathRIDs, nextArc.arcStopIDNext : buildPathSIDs, nextRID : buildPathRIDsPath))
            ) acc (filter (\rID -> head buildPathRIDsPath == rID || rID `Set.notMember` buildPathRIDs) nextArc.arcRoutesIDs)) currentPaths nextArcs
        where
          buildPathRIDsLength = Set.size buildPathRIDs
          buildPathSIDsLength = length buildPathSIDs
          currentSID = head buildPathSIDs

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRIDs, worstPathSIDs, _) = if isCurrentPathsComplete then last currentPaths else (Set.empty,[],[])
          worstPathRIDsLength = Set.size worstPathRIDs
          worstPathSIDsLength = length worstPathSIDs
    result = _findKPathsByLength [] (Set.singleton startSID) (Set.empty, [startSID], [0])

