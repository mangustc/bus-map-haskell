{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KPath
(
  findKPathsByLength,
  findKPathsByTransfers,
) where

import Structures
import qualified Data.Map as Map
import Data.List (insertBy, intersect, nub)
import Data.Ord (comparing)
import Debug.Trace (trace)

isPointlessAgainst :: ([RouteID], [StopID]) -> ([RouteID], [StopID]) -> Bool
isPointlessAgainst (pointlessRIDs, pointlessSIDs) (betterRIDs, betterSIDs) = let bRIDs = nub betterRIDs in
  length (bRIDs `intersect` pointlessRIDs) == length bRIDs && length pointlessSIDs >= length betterSIDs

findKPathsByLength :: Graph -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByLength = findKPaths insertSortedByLength

findKPathsByTransfers :: Graph -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPathsByTransfers = findKPaths insertSortedByTransfers

-- каждый раз как новый автобус проходится по остановке записывать его в список sIDsVisitedAmount, если автобусов там один, то максимум может быть pathAmount, если два автобуса, то максимум может быть pathAmount * 2 и так далее
findKPaths :: (QueueElement -> [QueueElement] -> [QueueElement]) -> Graph -> Int -> StopID -> StopID -> [([RouteID], [StopID])]
findKPaths insertFunction graph pathAmount startSID endSID = reverse (findKPathsByLength' [QueueElement {qeLengthCost = 0, qeTransferCost = 0, qeCurrentStopID = startSID, qePathRouteIDs = [0], qePathStopIDs = [startSID]}] Map.empty [])
  where
    findKPathsByLength' :: [QueueElement] -> Map.Map StopID Int -> [([RouteID], [StopID])] -> [([RouteID], [StopID])]
    findKPathsByLength' queue sIDsVisitedAmount currentPaths
      | length currentPaths >= pathAmount = currentPaths
      | null queue = currentPaths
      | otherwise =
          let currentEntry = head queue
              queue' = tail queue
              currentSIDVisitedAmount = Map.findWithDefault 0 currentEntry.qeCurrentStopID sIDsVisitedAmount
          in if currentSIDVisitedAmount >= pathAmount
             then findKPathsByLength' queue' sIDsVisitedAmount currentPaths
             else if currentEntry.qeCurrentStopID == endSID
                  then
                    let newPath = (reverse currentEntry.qePathRouteIDs, reverse currentEntry.qePathStopIDs)
                        isPathPointless = any (\path -> newPath `isPointlessAgainst` path) currentPaths
                        plus = if isPathPointless then 0 else 1
                        filteredPaths = if isPathPointless then trace (show currentEntry) currentPaths else newPath : filter (\path -> not (path `isPointlessAgainst` newPath)) currentPaths
                        newSIDsVisitedAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDVisitedAmount + plus) sIDsVisitedAmount
                    in findKPathsByLength' queue' newSIDsVisitedAmount filteredPaths
                  else
                    let nextSIDsWithWeights = Map.findWithDefault [] currentEntry.qeCurrentStopID graph
                        newEntries = [
                          QueueElement {
                            qeLengthCost = currentEntry.qeLengthCost + 1,
                            qeTransferCost = currentEntry.qeTransferCost + (if head currentEntry.qePathRouteIDs == nextRID then 0 else 1),
                            qeCurrentStopID = nextSID,
                            qePathRouteIDs = nextRID: currentEntry.qePathRouteIDs,
                            qePathStopIDs = nextSID : currentEntry.qePathStopIDs
                            } | (nextSID, nextRIDs) <- nextSIDsWithWeights, nextRID <- nextRIDs ]
                        newQueue = foldr insertFunction queue' newEntries
                        newSIDsVisitedAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDVisitedAmount + 1) sIDsVisitedAmount
                    in findKPathsByLength' newQueue newSIDsVisitedAmount currentPaths

