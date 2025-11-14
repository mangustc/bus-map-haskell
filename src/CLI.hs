module CLI (
  cliProcess,
  ) where

import KPath (
  findEveryKPath,
  getKPathsByLength,
  getKPathsByTransfers,
  QueueElement,
  )
import Structures
import Control.Monad.State
import Data.List (find, intercalate, isInfixOf, nub, sortBy)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO
import System.Exit
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Ord (comparing)

data CLISortType where
  SortByLength :: CLISortType
  SortByTransfers :: CLISortType
  deriving (Show, Read)
data CLIStopSelectionStopType where
  StopTypeStartStop :: CLIStopSelectionStopType
  StopTypeEndStop :: CLIStopSelectionStopType
  deriving (Show, Read)
data CLIScreen where
  CLIScreenRouteSelection :: CLIScreen
  CLIScreenMainMenu :: CLIScreen
  CLIScreenStopSelection :: CLIStopSelectionStopType ->
                              String ->
                              CLIScreen
  CLIScreenPathResults :: [QueueElement] -> CLISortType -> CLIScreen
  deriving (Show, Read)
data CLIState where
  CLIState :: {
    clisStops :: [Stop],
    clisRoutes :: [Route],
    clisGraph :: Graph,
    clisStartStopID :: StopID,
    clisEndStopID :: StopID,
    clisSelectedRouteIDs :: [RouteID],
    clisScreen :: CLIScreen,
    clisMessage :: String
  } -> CLIState
  deriving (Read, Show)
type CLIApp = StateT CLIState IO

pathAmount :: Int
pathAmount = 5

getStopIDName :: [Stop] -> StopID -> StopName
getStopIDName stops sID
  | sID == 0 = "Не выбрано"
  | otherwise = (getStopByStopID stops sID).stopName

getLineWithString :: String -> IO String
getLineWithString str = do
  putStr str
  hFlush stdout
  getLine

split :: Char -> String -> [String]
split _ [] = [""]
split delim (c:cs)
  | c == delim = "" : split delim cs
  | otherwise  = (c : head rest) : tail rest
  where
    rest = split delim cs

filterGraphRoutes :: [RouteID] -> Graph -> Graph
filterGraphRoutes allowedRoutesTemp = Map.map (filterNonEmpty . map filterEdgeRoutes)
  where
    allowedRoutes = 0 : allowedRoutesTemp
    filterEdgeRoutes :: (StopID, [RouteID]) -> (StopID, [RouteID])
    filterEdgeRoutes (stop, routes) = (stop, filter (`elem` allowedRoutes) routes)
    filterNonEmpty :: [(StopID, [RouteID])] -> [(StopID, [RouteID])]
    filterNonEmpty = filter (\(_, routes) -> not (null routes))


pathToConciseString :: [Stop] -> [Route] -> Path -> String
pathToConciseString stops routes path = segsToString path.pathConciseSegments
  where
    getRouteLine :: PathSegment -> String
    getRouteLine seg@(rID, _, _) = if seg `elem` path.pathFullSegments
                           then "--" ++ (getRouteByRouteID routes rID).routeName ++ "-->"
                           else "-...-" ++ (getRouteByRouteID routes rID).routeName ++ "-...->"
    segsToString :: [PathSegment] -> String
    segsToString [] = []
    segsToString [hseg@(_, sID, sIDNext)] = (getStopByStopID stops sID).stopName ++ " " ++ getRouteLine hseg ++ " " ++ (getStopByStopID stops sIDNext).stopName
    segsToString (hseg@(_, sID, _):tseg) = (getStopByStopID stops sID).stopName ++ " " ++ getRouteLine hseg ++ " " ++ segsToString tseg

getEveryQE :: Graph -> StopID -> StopID -> CLISortType -> [RouteID] -> [QueueElement]
getEveryQE graph startSID endSID sortType selectedRIDs = findEveryKPath filteredGraph pathAmount startSID endSID
  where
    filteredGraph = if null selectedRIDs then graph else filterGraphRoutes selectedRIDs graph

getStopWord :: Int -> String
getStopWord stopAmount
  | lastTwo >= 11 && lastTwo <= 14 = "остановок"
  | lastDigit == 1                 = "остановка"
  | lastDigit >= 2 && lastDigit <= 4 = "остановки"
  | otherwise                      = "остановок"
  where
    lastTwo = stopAmount `mod` 100
    lastDigit = stopAmount `mod` 10

getTransferWord :: Int -> String
getTransferWord n
  | lastTwo >= 11 && lastTwo <= 14 = "пересадок"
  | lastDigit == 1                 = "пересадка"
  | lastDigit >= 2 && lastDigit <= 4 = "пересадки"
  | otherwise                      = "пересадок"
  where
    lastTwo = n `mod` 100
    lastDigit = n `mod` 10

mainLoop :: CLIApp ()
mainLoop = do
  cliState <- get
  liftIO $ putStr "\ESC[2J"
  case cliState.clisScreen of
    CLIScreenMainMenu -> do
      liftIO $ putStrLn "Главное меню:\n"
      liftIO $ putStrLn ("Путь: " ++ getStopIDName cliState.clisStops cliState.clisStartStopID ++ " -> " ++ getStopIDName cliState.clisStops cliState.clisEndStopID)
      liftIO $ putStrLn ("Маршруты: " ++ if null cliState.clisSelectedRouteIDs then "Все" else intercalate ", " (map (\rID -> (getRouteByRouteID cliState.clisRoutes rID).routeName) cliState.clisSelectedRouteIDs) ++ "\n")
      liftIO $ putStrLn "1. Выбрать начальную остановку."
      liftIO $ putStrLn "2. Выбрать конечную остановку."
      liftIO $ putStrLn "3. Выбрать маршруты."
      liftIO $ putStrLn "4. Найти пути."
      liftIO $ putStrLn "0. Выйти из программы."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getLineWithString "Выберите вариант: "
      case choice of
        "1" -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeStartStop ""})
        "2" -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeEndStop ""})
        "3" -> do
          modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})
        "4" -> do
          if cliState.clisStartStopID == 0 || cliState.clisEndStopID == 0
          then modify (\clis -> clis {clisMessage = "\nНевозможно найти пути: необходимо выбрать начальную и конечную остановки.\n"})
          else modify (\clis -> clis {clisScreen = CLIScreenPathResults (getEveryQE cliState.clisGraph cliState.clisStartStopID cliState.clisEndStopID SortByLength cliState.clisSelectedRouteIDs) SortByLength})
        "0" -> do
          liftIO exitSuccess
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ choice ++ "\n"})
    CLIScreenStopSelection stopType filterName -> do
      liftIO $ putStrLn "Выбор остановки:\n"
      liftIO $ putStrLn (
        intercalate "\n" (
          map (\stop -> show stop.stopID ++ ". " ++ stop.stopName) (sortBy (comparing stopID)
            (filter (\stop -> let sName = takeWhile (/= '(') (map toLower stop.stopName)
                              in all (`isInfixOf` sName) (split ' ' (map toLower filterName))) cliState.clisStops))) ++ "\n")
      liftIO $ putStrLn "1. Отфильтровать по названию."
      liftIO $ putStrLn "2. Сбросить фильтр."
      liftIO $ putStrLn "3. Выбрать номер остановки."
      liftIO $ putStrLn "0. Вернуться в меню."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getLineWithString "Выберите вариант: "
      case choice of
        "1" -> do
          newFilterName <- liftIO $ getLineWithString "Введите запрос: "
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection stopType newFilterName})
        "2" -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection stopType ""})
        "3" -> do
          newStopIDString <- liftIO $ getLineWithString "Введите номер остановки: "
          case readMaybe newStopIDString of
            Just maybeStopID -> case find (\stop -> stop.stopID == maybeStopID) cliState.clisStops of
                                  Just _ -> case stopType of
                                    StopTypeStartStop -> modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisStartStopID = maybeStopID})
                                    StopTypeEndStop -> modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisEndStopID = maybeStopID})
                                  Nothing -> modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер остановки: " ++ newStopIDString ++ "\n"})
            Nothing -> modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер остановки: " ++ newStopIDString ++ "\n"})
        "0" -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ choice ++ "\n"})
    CLIScreenRouteSelection -> do
      liftIO $ putStrLn "Выбор маршрутов:\n"
      liftIO $ putStrLn (intercalate "\n" (map (\route -> show route.routeID ++ ". " ++ route.routeName) cliState.clisRoutes) ++ "\n")

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      newRIDsLine <- liftIO $ getLineWithString "Введите номер маршрутов через запятую (Например: \"1,2,3\" или оставить пустым для всех маршрутов): "
      newRIDs <- mapM (\rIDMaybe -> case readMaybe rIDMaybe of
                          Just rID -> case find (\route -> route.routeID == rID) cliState.clisRoutes of
                                        Just _ -> return rID
                                        Nothing -> do
                                          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер автобуса: " ++ rIDMaybe ++ "\n"})
                                          return 0
                          Nothing -> do
                            modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер автобуса: " ++ rIDMaybe ++ "\n"})
                            return 0
        ) (filter (/= "") (split ',' (filter (/= '\n') newRIDsLine)))
      if 0 `notElem` newRIDs then modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisSelectedRouteIDs = nub newRIDs}) else modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})

    CLIScreenPathResults paths sortBy -> do
      liftIO $ putStrLn ("Пути (по " ++ case sortBy of
                                         SortByLength -> "длине пути"
                                         SortByTransfers -> "количеству пересадок"
                                     ++ "):")
      liftIO $ putStrLn (intercalate "\n"
        (map (\(position, path) ->
          show position ++ ". " ++
          show path.pathLength ++ " " ++ getStopWord path.pathLength ++ ", " ++
          show path.pathTransferAmount ++ " " ++ getTransferWord path.pathTransferAmount ++ ", " ++
          show path.pathWalkingAmount ++ " " ++ "пешком" ++ ". " ++
          pathToConciseString cliState.clisStops (Route {routeID = 0, routeName = "пешком", routePath = []} : cliState.clisRoutes) path ++
          (if path.pathTransferAmount == 0 then "" else ".")
        ) (zip [1,2..] (case sortBy of
                        SortByLength -> getKPathsByLength paths
                        SortByTransfers -> getKPathsByTransfers paths))) ++ "\n")
      liftIO $ putStrLn ("1. Сортировать пути по " ++ case sortBy of
                                                        SortByLength -> "количеству пересадок"
                                                        SortByTransfers -> "длине пути"
                                                   ++ ".")
      liftIO $ putStrLn "0. Вернуться в меню."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getLineWithString "Выберите вариант: "
      case choice of
        "1" -> do
          let newSortBy = case sortBy of
                            SortByLength -> SortByTransfers
                            SortByTransfers -> SortByLength
          modify (\clis -> clis {clisScreen = CLIScreenPathResults paths newSortBy})
        "0" -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ choice ++ "\n"})
  mainLoop

cliProcess :: [Stop] -> [Route] -> IO ()
cliProcess stops routes = do
  liftIO $ putStr "\ESC[2J"
  let graph = edgesToGraph (edgesFromRoutesStops routes stops)
  let sIDsWithRoutes = Map.keys graph

  let cliState = CLIState {
    clisStops = filter (\s -> s.stopID `elem` sIDsWithRoutes) stops,
    clisRoutes = routes,
    clisGraph = graph,
    clisStartStopID = 0,
    clisEndStopID = 0,
    clisSelectedRouteIDs = [],
    clisScreen = CLIScreenMainMenu,
    clisMessage = ""
    }

  evalStateT mainLoop cliState



