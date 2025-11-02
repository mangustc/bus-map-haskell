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

split :: Char -> String -> [String]
split _ [] = [""]
split delim (c:cs)
  | c == delim = "" : split delim cs
  | otherwise  = (c : head rest) : tail rest
  where
    rest = split delim cs

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
          modify (\clis -> clis {clisScreen = CLIScreenRouteSelection})
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



