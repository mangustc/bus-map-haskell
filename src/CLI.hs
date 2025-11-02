module CLI (
  cliProcess,
  ) where

import KPath (
  findKPathsByLength,
  findKPathsByTransfers,
  )
import Structures
import Control.Monad.State
import Data.List (find, intercalate, isInfixOf)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.IO
import System.Exit

-- data AppState = AppState {
--     r
--     selectedRoutes :: [RouteID]
--     , startStopSelected :: Maybe Stop
--     , endStopSelected :: Maybe Stop
--     , allStops :: [Stop]
--     , currentFilter :: Maybe String
--     } deriving (Show)

data CLIFilterType =
  FilterByLength |
  FilterByTarnsfers
  deriving (Show, Read)
data CLIStopSelectionStopType =
  StopTypeStartStop |
  StopTypeEndStop
  deriving (Show, Read)
data CLIScreen =
  CLIScreenRouteSelection |
  CLIScreenMainMenu |
  CLIScreenStopSelection CLIStopSelectionStopType String |
  CLIScreenPathResults [Path] CLIFilterType
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

mainLoop :: CLIApp ()
mainLoop = do
  cliState <- get
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

      choice <- liftIO $ getCharNoNewline "Выберите вариант: "
      case choice of
        '1' -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeStartStop ""})
        '2' -> do
          modify (\clis -> clis {clisScreen = CLIScreenStopSelection StopTypeEndStop ""})
        '3' -> do
          modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})
        '4' -> do
          modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})
        'Q' -> do
          liftIO exitSuccess
        'q' -> do
          liftIO exitSuccess
        _ -> do
          modify (\clis -> clis {clisMessage = "Несуществующий вариант: " ++ (choice : "")})
    CLIScreenStopSelection stopType filterName -> do
      liftIO $ putStrLn "Выбор остановки:\n"
      liftIO $ putStrLn (intercalate "\n" (map (\stop -> show stop.stopID ++ ". " ++ stop.stopName) (filter (\stop -> filterName `isInfixOf` stop.stopName) cliState.clisStops)) ++ "\n")
      liftIO $ putStrLn "1. Отфильтровать по названию."
      liftIO $ putStrLn "2. Сбросить фильтр."
      liftIO $ putStrLn "3. Выбрать номер остановки."
      liftIO $ putStrLn "Q. Вернуться в меню."
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
                                  Just _ -> modify (\clis -> clis {clisScreen = CLIScreenMainMenu, clisStartStopID = maybeStopID})
                                  Nothing -> modify (\clis -> clis {clisMessage = "Несуществующий номер автобуса: " ++ newStopIDString})
            Nothing -> modify (\clis -> clis {clisMessage = "Несуществующий номер автобуса: " ++ newStopIDString})
        'Q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        'q' -> do
          modify (\clis -> clis {clisScreen = CLIScreenMainMenu})
        _ -> do
          modify (\clis -> clis {clisMessage = "Несуществующий вариант: " ++ (choice : "")})
    CLIScreenRouteSelection -> liftIO $ print "Route selection"
    CLIScreenPathResults paths filterBy -> liftIO $ print "Path Results"
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



