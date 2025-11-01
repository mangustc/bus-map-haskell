{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KPath
(
  findKPathsByLength,
  findKPathsByTransfers,
) where

import Structures
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (insertBy, intersect, nub)
import Data.Ord (comparing)
import Debug.Trace (trace)

isPointlessAgainst :: QueueElement -> QueueElement -> Bool
isPointlessAgainst pointlessQE betterQE = let bRIDs = nub betterQE.qePathRouteIDs in
  length (bRIDs `intersect` pointlessQE.qePathRouteIDs) == betterQE.qeTransferCost + 1 && pointlessQE.qeLengthCost >= betterQE.qeLengthCost

findKPathsByLength :: Graph -> Int -> StopID -> StopID -> [QueueElement]
findKPathsByLength = findKPaths insertSortedByLength

findKPathsByTransfers :: Graph -> Int -> StopID -> StopID -> [QueueElement]
findKPathsByTransfers = findKPaths insertSortedByTransfers

findKPaths :: (QueueElement -> [QueueElement] -> [QueueElement]) -> Graph -> Int -> StopID -> StopID -> [QueueElement]
findKPaths insertFunction graph pathAmount startSID endSID = map (\qe -> qe {qePathRouteIDs = tail (reverse qe.qePathRouteIDs), qePathStopIDs = reverse qe.qePathStopIDs}) $ reverse (findKPathsByLength' [QueueElement {qeLengthCost = 0, qeTransferCost = 0, qeCurrentStopID = startSID, qePathRouteIDs = [0], qePathStopIDs = [startSID]}] Map.empty [])
  where
    maximumRIDsAmount = length (nub (concatMap snd (concat (Map.elems graph))))
    findKPathsByLength' :: [QueueElement] -> Map.Map StopID Int -> [QueueElement] -> [QueueElement]
    findKPathsByLength' queue sIDsVisitedAmount currentPaths
      | length currentPaths >= pathAmount = currentPaths
      | null queue = currentPaths
      | otherwise =
          let currentEntry = head queue
              queue' = tail queue
              currentSIDVisitedAmount = Map.findWithDefault 0 currentEntry.qeCurrentStopID sIDsVisitedAmount
          in if currentSIDVisitedAmount >= pathAmount * (maximumRIDsAmount * 3)
             then findKPathsByLength' queue' sIDsVisitedAmount currentPaths
             else if currentEntry.qeCurrentStopID == endSID
                  then
                    let isPathPointless = any (\path -> currentEntry `isPointlessAgainst` path) currentPaths
                        plus = if isPathPointless then 0 else 1
                        filteredPaths = if isPathPointless then currentPaths else currentEntry : filter (\path -> not (path `isPointlessAgainst` currentEntry)) currentPaths
                        newSIDsVisitedAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDVisitedAmount + plus) sIDsVisitedAmount
                    in findKPathsByLength' queue' newSIDsVisitedAmount filteredPaths
                  else
                    let nextSIDsWithRIDs = Map.findWithDefault [] currentEntry.qeCurrentStopID graph
                        headRID = head currentEntry.qePathRouteIDs
                        newEntries = [
                          QueueElement {
                            qeLengthCost = currentEntry.qeLengthCost + 1,
                            qeTransferCost = nextTransferCost,
                            qeCurrentStopID = nextSID,
                            qePathRouteIDs = nextRID: currentEntry.qePathRouteIDs,
                            qePathStopIDs = nextSID : currentEntry.qePathStopIDs
                            } | (nextSID, nextRIDs) <- nextSIDsWithRIDs,
                            nextRID <- nextRIDs,
                            let nextTransferCost = currentEntry.qeTransferCost + (if headRID == nextRID then 0 else 1),
                            nextTransferCost <= maximumRIDsAmount &&
                              ((headRID == nextRID) || nextRID `notElem` currentEntry.qePathRouteIDs) &&
                              nextSID `notElem` currentEntry.qePathStopIDs ]
                        newQueue = foldr insertFunction queue' newEntries
                        newSIDsVisitedAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDVisitedAmount + 1) sIDsVisitedAmount
                    in findKPathsByLength' newQueue newSIDsVisitedAmount currentPaths

