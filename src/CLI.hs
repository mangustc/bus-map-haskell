module CLI (
  cliProcess,
  ) where

import KPath (
  findKPathsByLength,
  findKPathsByTransfers,
  )
import Structures
import Control.Monad.State
import Data.List (find, intercalate, isInfixOf, nub)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO
import System.Exit
import Debug.Trace (trace)
import qualified Data.Map as Map

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
  CLIScreenPathResults :: [Path] -> CLISortType -> CLIScreen
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

getCharNoNewline :: String -> IO Char
getCharNoNewline str = do
  putStr str
  hFlush stdout
  oldBuffering <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin oldBuffering
  putStrLn ""
  return c

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

-- Filter graph's routes by allowed routes list
filterGraphRoutes :: [RouteID] -> Graph -> Graph
filterGraphRoutes allowedRoutes = Map.map (filterNonEmpty . map filterEdgeRoutes)
  where
    filterEdgeRoutes :: (StopID, [RouteID]) -> (StopID, [RouteID])
    filterEdgeRoutes (stop, routes) =
      (stop, filter (`elem` allowedRoutes) routes)
    filterNonEmpty :: [(StopID, [RouteID])] -> [(StopID, [RouteID])]
    filterNonEmpty = filter (\(_, routes) -> not (null routes))


pathToConsiseString :: [Stop] -> [Route] -> Path -> String
pathToConsiseString stops routes path = segsToString path.pathConciseSegments
  where
    getRouteLine :: PathSegment -> String
    getRouteLine seg@(rID, _, _) = if seg `elem` path.pathFullSegments
                           then "--" ++ (getRouteByRouteID routes rID).routeName ++ "-->"
                           else "-...-" ++ (getRouteByRouteID routes rID).routeName ++ "-...->"
    segsToString :: [PathSegment] -> String
    segsToString [] = []
    segsToString [hseg@(_, sID, sIDNext)] = (getStopByStopID stops sID).stopName ++ " " ++ getRouteLine hseg ++ " " ++ (getStopByStopID stops sIDNext).stopName
    segsToString (hseg@(_, sID, _):tseg) = (getStopByStopID stops sID).stopName ++ " " ++ getRouteLine hseg ++ " " ++ segsToString tseg

getPathsBySortType :: Graph -> StopID -> StopID -> CLISortType -> [RouteID] -> [Path]
getPathsBySortType graph startSID endSID sortType selectedRIDs = (case sortType of
                                                                       SortByLength -> findKPathsByLength
                                                                       SortByTransfers -> findKPathsByTransfers) filteredGraph pathAmount startSID endSID
  where
    filteredGraph = if null selectedRIDs then graph else filterGraphRoutes selectedRIDs graph

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
      liftIO $ putStrLn "Q. Выйти из программы."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getCharNoNewline "Выберите вариант: "
      case choice of
        '1' -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeStartStop ""})
        '2' -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeEndStop ""})
        '3' -> do
          modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})
        '4' -> do
          if cliState.clisStartStopID == 0 || cliState.clisEndStopID == 0
          then modify (\clis -> clis {clisMessage = "\nНевозможно найти пути: необходимо выбрать начальную и конечную остановки.\n"})
          else modify (\clis -> clis {clisScreen = CLIScreenPathResults (getPathsBySortType cliState.clisGraph cliState.clisStartStopID cliState.clisEndStopID SortByLength cliState.clisSelectedRouteIDs) SortByLength})
        'Q' -> do
          liftIO exitSuccess
        'q' -> do
          liftIO exitSuccess
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ (choice : "") ++ "\n"})
    CLIScreenStopSelection stopType filterName -> do
      liftIO $ putStrLn "Выбор остановки:\n"
      liftIO $ putStrLn (intercalate "\n" (map (\stop -> show stop.stopID ++ ". " ++ stop.stopName) (filter (\stop -> filterName `isInfixOf` stop.stopName) cliState.clisStops)) ++ "\n")
      liftIO $ putStrLn "1. Отфильтровать по названию."
      liftIO $ putStrLn "2. Сбросить фильтр."
      liftIO $ putStrLn "3. Выбрать номер остановки."
      liftIO $ putStrLn "Q. Вернуться в меню."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getCharNoNewline "Выберите вариант: "
      case choice of
        '1' -> do
          newFilterName <- liftIO $ getLineWithString "Введите запрос: "
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection stopType newFilterName})
        '2' -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection stopType ""})
        '3' -> do
          newStopIDString <- liftIO $ getLineWithString "Введите номер остановки: "
          case readMaybe newStopIDString of
            Just maybeStopID -> case find (\stop -> stop.stopID == maybeStopID) cliState.clisStops of
                                  Just _ -> case stopType of
                                    StopTypeStartStop -> modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisStartStopID = maybeStopID})
                                    StopTypeEndStop -> modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisEndStopID = maybeStopID})
                                  Nothing -> modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер остановки: " ++ newStopIDString ++ "\n"})
            Nothing -> modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий номер остановки: " ++ newStopIDString ++ "\n"})
        'Q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        'q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ (choice : "") ++ "\n"})
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
      if 0 `notElem` newRIDs then modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisSelectedRouteIDs = newRIDs}) else modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})

    CLIScreenPathResults paths sortBy -> do
      liftIO $ putStrLn ("Пути (по " ++ case sortBy of
                                         SortByLength -> "длине пути"
                                         SortByTransfers -> "количеству пересадок"
                                     ++ "):")
      liftIO $ putStrLn (intercalate "\n" (map (\(position, path) -> show position ++ ". " ++ show path.pathLength ++ " остановок, " ++ show path.pathTransferAmount ++ " пересадок. " ++ pathToConsiseString cliState.clisStops cliState.clisRoutes path ++ ".") (zip [1,2..] paths)) ++ "\n")
      liftIO $ putStrLn ("1. Сортировать пути по " ++ case sortBy of
                                                        SortByLength -> "количеству пересадок"
                                                        SortByTransfers -> "длине пути"
                                                   ++ ".")
      liftIO $ putStrLn "Q. Вернуться в меню."

      liftIO $ putStr cliState.clisMessage
      modify (\clis -> clis {clisMessage = ""})
      choice <- liftIO $ getCharNoNewline "Выберите вариант: "
      case choice of
        '1' -> do
          let newSortBy = case sortBy of
                            SortByLength -> SortByTransfers
                            SortByTransfers -> SortByLength
          modify (\clis -> clis {clisScreen = CLIScreenPathResults (getPathsBySortType
                                                                      cliState.clisGraph
                                                                      cliState.clisStartStopID
                                                                      cliState.clisEndStopID
                                                                      newSortBy
                                                                      cliState.clisSelectedRouteIDs
                                                                      ) newSortBy})
        'Q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        'q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        _ -> do
          modify (\clis -> clis {clisMessage = "\n" ++ "Несуществующий вариант: " ++ (choice : "") ++ "\n"})
  mainLoop

cliProcess :: [Stop] -> [Route] -> IO ()
cliProcess stops routes = do
  let arcs = arcsFromRoutes routes
  let graph = arcsToGraph arcs

  let cliState = CLIState {
    clisStops = stops,
    clisRoutes = routes,
    clisGraph = graph,
    clisStartStopID = 0,
    clisEndStopID = 0,
    clisSelectedRouteIDs = [],
    clisScreen = CLIScreenMainMenu,
    clisMessage = ""
    }

  evalStateT mainLoop cliState



