{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KPath
(
  findKPathsByLength,
  findKPathsByTransfers,
) where

import Structures
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (insertBy, intersect, nub, sortBy, nubBy)
import Data.Ord (comparing)

type Weight = Int
data QueueElement where
  QueueElement :: {
    qeTransferCost :: Weight,
    qeLengthCost :: Weight,
    qeCurrentStopID :: StopID,
    qePathStopIDs :: [StopID],
    qePathRouteIDs :: [RouteID]
  } -> QueueElement
  deriving (Read, Show)

insertSortedByLength :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByLength = insertBy (\qe1 qe2 -> comparing qeLengthCost qe1 qe2 <> comparing qeTransferCost qe1 qe2)

insertSortedByTransfers :: QueueElement -> [QueueElement] -> [QueueElement]
insertSortedByTransfers = insertBy (\qe1 qe2 -> comparing qeTransferCost qe1 qe2 <> comparing qeLengthCost qe1 qe2)

queueElementToPath :: QueueElement -> Path
queueElementToPath qe = Path {
  pathLength = qe.qeLengthCost,
  pathTransferAmount = qe.qeTransferCost - 1,
  pathFullSegments = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs)),
  pathConciseSegments = getConciseSegments fullSegs
  }
  where
    getConciseSegments :: [PathSegment] -> [PathSegment]
    getConciseSegments [] = []
    getConciseSegments [hps] = [hps]
    getConciseSegments (hps@(hRID, hSID, hSIDNext):tps)
      | nRID == hRID = (hRID, hSID, nSIDNext) : tail next
      | otherwise = hps : next
      where
        next = getConciseSegments tps
        (nRID,nSID,nSIDNext) = head next

    fullSegs = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs))

isPointlessAgainst :: QueueElement -> QueueElement -> Bool
isPointlessAgainst pointlessQE betterQE =
  let bRIDs = nub betterQE.qePathRouteIDs
      pRIDs = nub pointlessQE.qePathRouteIDs
  in length (bRIDs `intersect` pRIDs) == length bRIDs && pointlessQE.qeLengthCost >= betterQE.qeLengthCost

findEveryKPath :: Graph -> Int -> StopID -> StopID -> [QueueElement]
findEveryKPath graph pathAmount startSID endSID = if length everyPath == 1 then everyPath else filter (\qe -> nub qe.qePathRouteIDs /= [0]) everyPath
  where
    everyPath = nubBy (\qe1 qe2 -> qe1.qePathRouteIDs == qe2.qePathRouteIDs && qe1.qePathStopIDs == qe2.qePathStopIDs)
      (findKPaths insertSortedByLength graph pathAmount startSID endSID ++ findKPaths insertSortedByTransfers graph pathAmount startSID endSID)

findKPathsByLength :: Graph -> Int -> StopID -> StopID -> [Path]
findKPathsByLength graph pathAmount startSID endSID = map queueElementToPath
  (take pathAmount
    (sortBy (\qe1 qe2 -> comparing qeLengthCost qe1 qe2 <> comparing qeTransferCost qe1 qe2)
      (findEveryKPath graph pathAmount startSID endSID)))

findKPathsByTransfers :: Graph -> Int -> StopID -> StopID -> [Path]
findKPathsByTransfers graph pathAmount startSID endSID = map queueElementToPath
  (take pathAmount
    (sortBy (\qe1 qe2 -> comparing qeTransferCost qe1 qe2 <> comparing qeLengthCost qe1 qe2)
      (findEveryKPath graph pathAmount startSID endSID)))

findKPaths :: (QueueElement -> [QueueElement] -> [QueueElement]) -> Graph -> Int -> StopID -> StopID -> [QueueElement]
findKPaths insertFunction graph pathAmount startSID endSID = map
  (\qe -> qe {qePathRouteIDs = tail (reverse qe.qePathRouteIDs), qePathStopIDs = reverse qe.qePathStopIDs})
    $ reverse (findKPathsByLength' [QueueElement {qeLengthCost = 0, qeTransferCost = 0, qeCurrentStopID = startSID, qePathRouteIDs = [-1], qePathStopIDs = [startSID]}] Map.empty [])
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
                              qeLengthCost = currentEntry.qeLengthCost + if nextRID == 0 then 3 else 1,
                              qeTransferCost = nextTransferCost,
                              qeCurrentStopID = nextSID,
                              qePathRouteIDs = nextRID: currentEntry.qePathRouteIDs,
                              qePathStopIDs = nextSID : currentEntry.qePathStopIDs
                            } | (nextSID, nextRIDs) <- nextSIDsWithRIDs,
                            nextRID <- nextRIDs,
                            let nextTransferCost = currentEntry.qeTransferCost + (if headRID == nextRID then 0 else 1),
                            nextTransferCost <= maximumRIDsAmount + 1 &&
                              nextSID `notElem` currentEntry.qePathStopIDs ]
                        newQueue = foldr insertFunction queue' newEntries
                        newSIDsVisitedAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDVisitedAmount + 1) sIDsVisitedAmount
                    in findKPathsByLength' newQueue newSIDsVisitedAmount currentPaths

