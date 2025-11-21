{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module KPath
(
  findEveryKPath,
  getKPathsByTransfers,
  getKPathsByLength,
  QueueElement,
) where

import Structures
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (insertBy, intersect, nub, sortBy, nubBy, minimumBy, maximumBy, isInfixOf)
import Data.Ord (comparing)
import Debug.Trace (trace)

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
    pathLength = max 0 (length qe.qePathStopIDs - 1),
    pathTransferAmount = max 0 (length (filter (/= 0) (cutRepeating qe.qePathRouteIDs)) - 1),
    pathWalkingAmount = length (filter (== 0) qe.qePathRouteIDs),
    pathFullSegments = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs)),
    pathConciseSegments = getConciseSegments fullSegs
  }
  where
    getConciseSegments :: [PathSegment] -> [PathSegment]
    getConciseSegments [] = []
    getConciseSegments [hps] = [hps]
    getConciseSegments (hps@(hRID, hSID, hSIDNext):tps)
      | nRID /= hRID || nRID == 0 = hps : next
      | otherwise = (hRID, hSID, nSIDNext) : tail next
      where
        next = getConciseSegments tps
        (nRID,nSID,nSIDNext) = head next

    fullSegs = zipWith (\rID (sID, sIDNext) -> (rID, sID, sIDNext)) qe.qePathRouteIDs (zip qe.qePathStopIDs (tail qe.qePathStopIDs))

cutRepeating :: [RouteID] -> [RouteID]
cutRepeating [] = []
cutRepeating [rID] = [rID]
cutRepeating (rID : rIDs)
  | head nextCutRepeating == rID = nextCutRepeating
  | otherwise = rID : nextCutRepeating
  where
    nextCutRepeating = cutRepeating rIDs

removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne x (y:ys) | x == y = ys
removeOne x (y:ys) = y : removeOne x ys

isPointlessAgainst :: QueueElement -> QueueElement -> Bool
isPointlessAgainst pointlessQE betterQE =
  let bRIDs = cutRepeating betterQE.qePathRouteIDs
      pRIDs = cutRepeating pointlessQE.qePathRouteIDs
  in null (foldr removeOne bRIDs pRIDs) && (pointlessQE.qeLengthCost >= betterQE.qeLengthCost)

maxZerosPercentage :: Double
maxZerosPercentage = 0.5

findEveryKPath :: Graph -> Int -> StopID -> StopID -> [QueueElement]
findEveryKPath graph pathAmount startSID endSID = filter (\qe ->
  not (any (\qe2 -> not (qe.qePathRouteIDs == qe2.qePathRouteIDs && qe.qePathStopIDs == qe2.qePathStopIDs) && qe `isPointlessAgainst` qe2) maybeWithoutZeroPaths)) maybeWithoutZeroPaths
  where
    everyPath = nubBy (\qe1 qe2 -> qe1.qePathRouteIDs == qe2.qePathRouteIDs && qe1.qePathStopIDs == qe2.qePathStopIDs)
      (findKPaths insertSortedByLength graph pathAmount startSID endSID ++ findKPaths insertSortedByTransfers graph pathAmount startSID endSID)
    averageLengthCost = sum (map qeLengthCost everyPath) `div` length everyPath
    maybeWithoutZeroPaths = if length everyPath /= 1
                            then filter (\qe -> let zerosAmount = length (filter (== 0) qe.qePathRouteIDs)
                                                    routesAmount = length qe.qePathRouteIDs
                                                    zerosPercentage = (fromIntegral zerosAmount / fromIntegral routesAmount)
                                                in not (averageLengthCost < qe.qeLengthCost && (zerosPercentage > maxZerosPercentage))) everyPath
                            else everyPath

getKPathsByLength :: [QueueElement] -> [Path]
getKPathsByLength qePaths = map queueElementToPath sortedPaths
  where
    sortedPaths = sortBy (\qe1 qe2 -> comparing qeLengthCost qe1 qe2 <> comparing (\qe -> max 0 (length (filter (/= 0) (cutRepeating qe.qePathRouteIDs)) - 1)) qe1 qe2) qePaths

getKPathsByTransfers :: [QueueElement] -> [Path]
getKPathsByTransfers qePaths = map queueElementToPath sortedPaths
  where
    sortedPaths = sortBy (\qe1 qe2 -> comparing (\qe -> max 0 (length (filter (/= 0) (cutRepeating qe.qePathRouteIDs)) - 1)) qe1 qe2 <> comparing qeLengthCost qe1 qe2) qePaths


getSIDsRemainingAmounts :: Map.Map StopID [(StopID, [RouteID])] -> Int -> Map.Map StopID Int
getSIDsRemainingAmounts graph pathAmount =
  let destRoutes = concatMap (\(_, tos) -> [(to, rid) | (to, rids) <- tos, rid <- rids]) (Map.toList graph)
      inRoutes = map (\(sID, tos) -> (sID, concatMap snd tos)) (Map.toList graph)
      routeMap = Map.fromListWith (++) ([(to, [rid]) | (to, rid) <- destRoutes] ++ inRoutes)
  in Map.map (\rIDs -> length rIDs * pathAmount) routeMap

findKPaths :: (QueueElement -> [QueueElement] -> [QueueElement]) -> Graph -> Int -> StopID -> StopID -> [QueueElement]
findKPaths insertFunction graph pathAmountTemp startSID endSID = map
  (\qe -> qe {qePathRouteIDs = tail (reverse qe.qePathRouteIDs), qePathStopIDs = reverse qe.qePathStopIDs})
    $ reverse (findKPaths' [QueueElement {
                              qeLengthCost = 0,
                              qeTransferCost = 0,
                              qeCurrentStopID = startSID,
                              qePathRouteIDs = [-1],
                              qePathStopIDs = [startSID]
                            }] (getSIDsRemainingAmounts graph pathAmount) [])
  where
    pathAmount = pathAmountTemp + 1
    maximumRIDsAmount = length (nub (concatMap snd (concat (Map.elems graph))))
    findKPaths' :: [QueueElement] -> Map.Map StopID Int -> [QueueElement] -> [QueueElement]
    findKPaths' queue sIDsRemainingAmount currentPaths
      | length currentPaths >= pathAmount = currentPaths
      | null queue = currentPaths
      | otherwise =
          let currentEntry = head queue
              queue' = tail queue
              currentSIDRemainingAmount = Map.findWithDefault 0 currentEntry.qeCurrentStopID sIDsRemainingAmount
          in if currentEntry.qeCurrentStopID == endSID
                  then
                    let newCurrentPaths = currentEntry : filter (\path -> not (path `isPointlessAgainst` currentEntry)) currentPaths
                        newSIDsRemainingAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDRemainingAmount - 1) sIDsRemainingAmount
                    in findKPaths' queue' newSIDsRemainingAmount newCurrentPaths
                  else
                    let nextSIDsWithRIDs = Map.findWithDefault [] currentEntry.qeCurrentStopID graph
                        headRID = head currentEntry.qePathRouteIDs
                        newEntries = [
                          qe | (nextSID, nextRIDs) <- nextSIDsWithRIDs,
                          nextRID <- nextRIDs,
                          let nextTransferCost = currentEntry.qeTransferCost + (if headRID == nextRID then 0 else 1),
                          let qe = QueueElement {
                            qeLengthCost = currentEntry.qeLengthCost + if nextRID == 0 then 4 else 1,
                            qeTransferCost = nextTransferCost,
                            qeCurrentStopID = nextSID,
                            qePathRouteIDs = nextRID : currentEntry.qePathRouteIDs,
                            qePathStopIDs = nextSID : currentEntry.qePathStopIDs
                          },
                          nextTransferCost <= maximumRIDsAmount &&
                            (nextRID == 0 || headRID == nextRID || nextRID `notElem` tail currentEntry.qePathRouteIDs) &&
                            nextSID `notElem` currentEntry.qePathStopIDs &&
                            not (any (\path -> qe `isPointlessAgainst` path) currentPaths) &&
                            not (any (\path -> head path.qePathStopIDs == head qe.qePathStopIDs && qe `isPointlessAgainst` path) queue')
                          ]
                        newQueue = foldr insertFunction queue' newEntries
                        newSIDsRemainingAmount = Map.insert currentEntry.qeCurrentStopID (currentSIDRemainingAmount - 1) sIDsRemainingAmount
                    in findKPaths' newQueue newSIDsRemainingAmount currentPaths

