module Main where
import Control.Monad
import Data.Maybe
import Statistics.Function
import Statistics.Sample
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Lib
import TektronixFormat

data PrgMode = Trigger | Period 
  deriving (Show, Eq)
data Options = Options { optVerbose :: Bool
                       , optMode :: PrgMode
                       }

startOptions = Options { optVerbose = False
                       , optMode  = Period
                       }

basicUsage prgname = 
  prgname ++ ": tools to analyze jitter with Tektronix oscilloscope\n\n"
  ++ "Usage for trigger-mode: \n"
  ++ "\t$ " ++ prgname ++ " -t {threshold1} {data1} {threshold2} {data2}\n"
  ++ "Usage for period-mode: \n"
  ++ "\t$ " ++ prgname ++ " -p {threshold} {data}\n"

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "v" ["verbose"] (NoArg (\opt -> return opt {optVerbose = True})) "Enable verbose message"
          , Option "t" ["trigger"] (NoArg (\opt -> return opt {optMode = Trigger})) "Analysis mode: Trigger -> relation of 2 channels"
          , Option "p" ["period"] (NoArg (\opt -> return opt {optMode = Period})) "Analysis mode: Period -> just measure 1 channel and calculate its period"
          , Option "h" ["help"]
            (NoArg
              (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitSuccess))
            "Show help"
          ]


main :: IO ()
main = do
  args <- getArgs
  let (actions, nonoptArg, errs) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) actions
  progname  <- getProgName
  let Options { optVerbose = verbose
              , optMode = mode
              } = opts
  case mode of 
    Trigger -> do
      if length nonoptArg < 4
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          dataFileName1 = nonoptArg !! 1
          dataFileName2 = nonoptArg !! 3
          threshold1 = head nonoptArg
          threshold2 = nonoptArg !! 2
        when verbose $ hPutStrLn stderr $ "threshold1: " ++ show threshold1 ++ ", file1: " ++ dataFileName1 ++ ", threshold2: " ++ show threshold2 ++ ", file2: " ++ dataFileName2

        flagDoesFile1 <- doesFileExist dataFileName1
        flagDoesFile2 <- doesFileExist dataFileName2
        dataFileNames <- if flagDoesFile1 && flagDoesFile2
                            then return [dataFileName1, dataFileName2]
                            else return []
        f1 <- readTektronixFile (head dataFileNames)
        f2 <- readTektronixFile (dataFileNames !! 1)
        let
          xpoints1 = flatInTime $ findCrossPoints (read threshold1) ((takeTimeVoltageCurve . parseTektronixFormat) f1)
          xpoints2 = flatInTime $ findCrossPoints (read threshold2) ((takeTimeVoltageCurve . parseTektronixFormat) f2)
        print $ compCenterEdge xpoints1 xpoints2
        exitSuccess
    Period -> do
      if length nonoptArg < 2
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          dataFileName = nonoptArg !! 1
          threshold = head nonoptArg
        when verbose $ hPutStrLn stderr $ "threshold: " ++ show threshold ++ ", file: " ++ dataFileName
        flagDoesFile <- doesFileExist dataFileName
        if not flagDoesFile then
          exitFailure
        else do
            f <- readTektronixFile dataFileName
            mapM_ print $ takeDiff . flatInTime $ findCrossPoints (read threshold) ((takeTimeVoltageCurve . parseTektronixFormat) f)
            exitSuccess

  
takeDiff :: (Num a) => [a] -> [a]
takeDiff (x:y:xs) = y-x : takeDiff (y:xs)
takeDiff [x] = []
takeDiff [] = []

