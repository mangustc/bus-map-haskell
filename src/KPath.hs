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
  insertBy,
  )
import Data.Ord (
  comparing,
  )
import qualified Data.Set as Set

isPointlessAgainst :: (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> Bool
isPointlessAgainst (pointlessRIDs, pointlessSIDs, _, _) (betterRIDs, betterSIDs, _, _) =
  betterRIDs `Set.isSubsetOf` pointlessRIDs && Set.size pointlessSIDs >= Set.size betterSIDs

insertCurrentPathsTransfers :: (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])] -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])]
insertCurrentPathsTransfers buildPath@(buildPathRIDs, buildPathSIDs, _, _) xs =
  let buildPathSIDsLength = Set.size buildPathSIDs
      buildPathRIDsLength = Set.size buildPathRIDs
      insertByCached [] = [buildPath]
      insertByCached all@(y@(pRIDs, pSIDs, _, _):ys)
        | (compare buildPathRIDsLength (Set.size pRIDs) <> compare buildPathSIDsLength (Set.size pSIDs)) == LT = buildPath : all
        | otherwise = y : insertByCached ys
  in insertByCached xs

findKPathsByTransfers :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByTransfers arcs pathAmount startSID endSID = map (\(_, _, rIDs, sIDs) -> (tail (reverse rIDs), reverse sIDs)) result
  where
    maximumRIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByTransfers :: [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])] -> Set.Set StopID -> (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])]
    _findKPathsByTransfers currentPaths visitedStopIDs buildPath@(buildPathRIDs, buildPathSIDs, buildPathRIDsPath, buildPathSIDsPath)
      -- Reasons to end search early
      | buildPathRIDsLength > maximumRIDsAmount = currentPaths
      | isCurrentPathsComplete && (buildPathRIDsLength > worstPathRIDsLength || (buildPathRIDsLength == worstPathRIDsLength) && (buildPathSIDsLength > worstPathSIDsLength)) = currentPaths
      | any (\path -> buildPath `isPointlessAgainst` path) currentPaths = currentPaths

      -- has come to the end
      | currentSID == endSID =
          let newPaths = insertCurrentPathsTransfers buildPath (filter (\path -> not (path `isPointlessAgainst` buildPath)) currentPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths

      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentSID && arc.arcStopIDNext `Set.notMember` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            (_findKPathsByTransfers acc2 (Set.insert nextArc.arcStopIDNext visitedStopIDs) (Set.insert nextRID buildPathRIDs, Set.insert nextArc.arcStopIDNext buildPathSIDs, nextRID : buildPathRIDsPath, nextArc.arcStopIDNext : buildPathSIDsPath))
            ) acc (filter (\rID -> head buildPathRIDsPath == rID || rID `Set.notMember` buildPathRIDs) nextArc.arcRoutesIDs)) currentPaths nextArcs
        where
          buildPathRIDsLength = Set.size buildPathRIDs
          buildPathSIDsLength = Set.size buildPathSIDs
          currentSID = head buildPathSIDsPath

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRIDs, worstPathSIDs, _, _) = if isCurrentPathsComplete then last currentPaths else (Set.empty,Set.empty,[],[])
          worstPathRIDsLength = Set.size worstPathRIDs
          worstPathSIDsLength = Set.size worstPathSIDs
    result = _findKPathsByTransfers [] (Set.singleton startSID) (Set.empty, Set.singleton startSID, [0], [startSID])

insertCurrentPathsLength :: (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])] -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])]
insertCurrentPathsLength buildPath@(buildPathRIDs, buildPathSIDs, _, _) xs =
  let buildPathSIDsLength = Set.size buildPathSIDs
      buildPathRIDsLength = Set.size buildPathRIDs
      insertByCached [] = [buildPath]
      insertByCached all@(y@(pRIDs, pSIDs, _, _):ys)
        | (compare buildPathSIDsLength (Set.size pSIDs) <> compare buildPathRIDsLength (Set.size pRIDs) ) == LT = buildPath : all
        | otherwise = y : insertByCached ys
  in insertByCached xs

findKPathsByLength :: [Arc] -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByLength arcs pathAmount startSID endSID = map (\(_, _, rIDs, sIDs) -> (tail (reverse rIDs), reverse sIDs)) result
  where
    maximumRIDsAmount = length (nub (concatMap arcRoutesIDs arcs))
    _findKPathsByLength :: [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])] -> Set.Set StopID -> (Set.Set RouteID, Set.Set StopID, [RouteID], [StopID]) -> [(Set.Set RouteID, Set.Set StopID, [RouteID], [StopID])]
    _findKPathsByLength currentPaths visitedStopIDs buildPath@(buildPathRIDs, buildPathSIDs, buildPathRIDsPath, buildPathSIDsPath)
      -- Reasons to end search early
      | buildPathRIDsLength > maximumRIDsAmount = currentPaths
      | isCurrentPathsComplete && (buildPathSIDsLength > worstPathSIDsLength || (buildPathSIDsLength == worstPathSIDsLength) && (buildPathRIDsLength > worstPathRIDsLength)) = currentPaths
      | any (\path -> buildPath `isPointlessAgainst` path) currentPaths = currentPaths

      -- has come to the end
      | currentSID == endSID =
          let newPaths = insertCurrentPathsLength buildPath (filter (\path -> not (path `isPointlessAgainst` buildPath)) currentPaths)
          in if isCurrentPathsComplete then take pathAmount newPaths else newPaths

      | otherwise =
          let nextArcs = filter (\arc -> arc.arcStopID == currentSID && arc.arcStopIDNext `Set.notMember` visitedStopIDs) arcs
          in foldl' (\acc nextArc -> foldl' (\acc2 nextRID ->
            (_findKPathsByLength acc2 (Set.insert nextArc.arcStopIDNext visitedStopIDs) (Set.insert nextRID buildPathRIDs, Set.insert nextArc.arcStopIDNext buildPathSIDs, nextRID : buildPathRIDsPath, nextArc.arcStopIDNext : buildPathSIDsPath))
            ) acc (filter (\rID -> head buildPathRIDsPath == rID || rID `Set.notMember` buildPathRIDs) nextArc.arcRoutesIDs)) currentPaths nextArcs
        where
          buildPathRIDsLength = Set.size buildPathRIDs
          buildPathSIDsLength = Set.size buildPathSIDs
          currentSID = head buildPathSIDsPath

          isCurrentPathsComplete = length currentPaths == pathAmount
          (worstPathRIDs, worstPathSIDs, _, _) = if isCurrentPathsComplete then last currentPaths else (Set.empty,Set.empty,[],[])
          worstPathRIDsLength = Set.size worstPathRIDs
          worstPathSIDsLength = Set.size worstPathSIDs
    result = _findKPathsByLength [] (Set.singleton startSID) (Set.empty, Set.singleton startSID, [0], [startSID])

