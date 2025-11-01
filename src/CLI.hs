module CLI (
  cliProcess,
  ) where

import KPath (
  findKPathsByLength,
  findKPathsByTransfers,
  )
import Structures
import Control.Monad.State
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe)
import System.IO

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
data CLIScreen =
  RouteSelection |
  MainMenu |
  StopSelection |
  PathResults
  deriving (Show, Read)
data CLIState where
  CLIState :: {
    clisStops :: [Stop],
    clisRoutes :: [Route],
    clisGraph :: Graph,
    clisStartStopID :: StopID,
    clisEndStopID :: StopID,
    clisSelectedRouteIDs :: [RouteID],
    clisFilterType :: CLIFilterType,
    clisPaths :: [Path],
    clisScreen :: CLIScreen,
    clisMessage :: String
  } -> CLIState
  deriving (Read, Show)

type CLIApp = StateT CLIState IO

getStopIDName :: [Stop] -> StopID -> StopName
getStopIDName stops sID
  | sID == 0 = "Не выбрано"
  | otherwise = (getStopByStopID stops sID).stopName

getCharNoNewline :: IO Char
getCharNoNewline = do
  oldBuffering <- hGetBuffering stdin
  oldEcho <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetBuffering stdin oldBuffering
  hSetEcho stdin oldEcho
  return c

mainLoop :: CLIApp ()
mainLoop = do
  cliState <- get
  case cliState.clisScreen of
    MainMenu -> do
      liftIO $ putStrLn ("Путь: " ++ getStopIDName cliState.clisStops cliState.clisStartStopID ++ " -> " ++ getStopIDName cliState.clisStops cliState.clisEndStopID)
      liftIO $ putStrLn ("Маршруты: " ++ if null cliState.clisSelectedRouteIDs then "Все" else intercalate ", " (map (\rID -> (getRouteByRouteID cliState.clisRoutes rID).routeName) cliState.clisSelectedRouteIDs))
      liftIO $ putStrLn "1. Выбрать начальную остановку."
      liftIO $ putStrLn "2. Выбрать конечную остановку."
      liftIO $ putStrLn "3. Выбрать маршруты."
      liftIO $ putStrLn "4. Найти пути."
      liftIO $ putStrLn "Q. Выйти из программы."

      choice <- liftIO getCharNoNewline
      case choice of
        '1' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        '2' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        '3' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        '4' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        'Q' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        'q' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
        '_' -> do
          modify (\clis -> clis {clisScreen = RouteSelection})
    RouteSelection -> liftIO $ print "Route selection"
    StopSelection -> liftIO $ print "Stop Selection"
    PathResults -> liftIO $ print "Path Results"
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
    clisFilterType = FilterByLength,
    clisPaths = [],
    clisScreen = MainMenu,
    clisMessage = ""
    }

  evalStateT mainLoop cliState



