module Main where
import Control.Monad
import Data.Maybe
import GHC.Natural
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import TektronixFormat

data PrgMode = Trigger | Period | TIE
  deriving (Show, Eq)
data TIEMode = TIELocal | TIEGlobal
  deriving (Show, Eq)
data Options = Options { optVerbose :: Bool
                       , optMode :: PrgMode
                       , optStartEpochTime :: Int
                       , optIdealPeriod :: Double
                       , optTIEGL :: TIEMode
                       } deriving (Show)

startOptions = Options { optVerbose = False
                       , optMode  = Period
                       , optStartEpochTime = 1614135600 -- 2021-02-24 12:00:00
                       , optIdealPeriod = 8e-9
                       , optTIEGL = TIELocal
                       }

basicUsage prgname = 
  prgname ++ ": tools to analyze jitter with Tektronix oscilloscope\n\n"
  ++ "Usage for trigger-mode: \n"
  ++ "\t$ " ++ prgname ++ " -t {threshold1} {data1} {threshold2} {data2}\n"
  ++ "Usage for period-mode: \n"
  ++ "\t$ " ++ prgname ++ " -p {threshold} {data}\n"
  ++ "Usage for tie-mode: \n"
  ++ "\t$ " ++ prgname ++ " --tie {threshold} {data}" -- [threshold2] [data2]\n"

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "v" ["verbose"] (NoArg (\opt -> return opt {optVerbose = True})) "Enable verbose message"
          , Option "t" ["trigger"] (NoArg (\opt -> return opt {optMode = Trigger})) "Analysis mode: Trigger -> relation of 2 channels"
          , Option "p" ["period"] (NoArg (\opt -> return opt {optMode = Period})) "Analysis mode: Period -> just measure 1 channel and calculate its period"
          , Option "" ["tie"] (NoArg (\opt -> return opt {optMode = TIE})) "Analysis mode: TIE -> calculate difference from ideal clcok edge"
          , Option "" ["tstart"]
            (ReqArg 
              (\arg opt -> return opt { optStartEpochTime = read arg, optTIEGL = TIEGlobal }) "TIME")
            "For TIE analysis: base time in epoch format as global"
          , Option "" ["idealperiod"]
            (ReqArg 
              (\arg opt -> return opt { optIdealPeriod = read arg}) "TIME")
            "For TIE analysis: ideal period in sec"
          , Option "" ["localtie"] (NoArg (\opt -> return opt {optTIEGL = TIELocal})) "TIE ideal edge origin : set Local"
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
              , optStartEpochTime = tstart
              , optIdealPeriod = idealPeriod
              , optTIEGL = tieMode
              } = opts
      files = parseFileArgs nonoptArg :: [(Double, String)]
  when verbose $ hPrint stderr opts
  case mode of 
    Trigger -> do
      if length files < 2
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          ( threshold1, dataFileName1 ) = head files
          ( threshold2, dataFileName2 ) = head files
        when verbose $ hPutStrLn stderr $ "threshold1: " ++ show threshold1 ++ ", file1: " ++ dataFileName1 ++ ", threshold2: " ++ show threshold2 ++ ", file2: " ++ dataFileName2

        flagDoesFile1 <- doesFileExist dataFileName1
        flagDoesFile2 <- doesFileExist dataFileName2
        dataFileNames <- if flagDoesFile1 && flagDoesFile2
                            then return [dataFileName1, dataFileName2]
                            else return []
        f1 <- readTektronixFile (head dataFileNames)
        f2 <- readTektronixFile (dataFileNames !! 1)
        let
          xpoints1 = flatInTime $ findCrossPoints threshold1 (takeTimeVoltageCurve f1)
          xpoints2 = flatInTime $ findCrossPoints threshold2 (takeTimeVoltageCurve f2)
        print $ compCenterEdge xpoints1 xpoints2
        exitSuccess
    Period -> do
      if length nonoptArg < 2
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          ( threshold, dataFileName ) = head files
        when verbose $ hPutStrLn stderr $ "threshold: " ++ show threshold ++ ", file: " ++ dataFileName
        flagDoesFile <- doesFileExist dataFileName
        if not flagDoesFile then
          exitFailure
        else do
            f <- readTektronixFile dataFileName
            mapM_ print $ takeDiff . flatInTime $ findCrossPoints threshold (takeTimeVoltageCurve f)
            exitSuccess
    TIE -> do
      if length nonoptArg < 2
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          ( threshold, dataFileName ) = head files
        when verbose $ hPutStrLn stderr $ "threshold: " ++ show threshold ++ ", file: " ++ dataFileName
        flagDoesFile <- doesFileExist dataFileName
        if not flagDoesFile then
          exitFailure
        else do
            f <- readTektronixFile dataFileName
            let
              sec = (gmtSec . header) f - tstart
              diffEpochTime = if tieMode == TIEGlobal then fromIntegral sec  + (fracSec . header) f else 0
              idealEdge = generateTrueCLKEdge' (diffEpochTime + (impDimOffset . impDim1 . header) f) 0 idealPeriod
              delta = ((head . takeDiff . take 2 ) idealEdge - idealPeriod) / idealPeriod
              epsilon = 1e-3
            when verbose $ hPutStrLn stderr $ "header GMT time = " ++ (show . gmtSec . header) f
            when (epsilon < delta) $ hPutStrLn stderr $ "Warning: neumerical digits-loss in grobal clock edge calculation: " ++ show delta
            mapM_ printDP $ zipWith (\x y -> (x, x-y)) (map (+ diffEpochTime) $ flatInTime $ findCrossPoints threshold (takeTimeVoltageCurve f)) idealEdge
              where printDP (x,y) = putStrLn $ show x ++ " " ++ show y

  
takeDiff :: (Num a) => [a] -> [a]
takeDiff (x:y:xs) = y-x : takeDiff (y:xs)
takeDiff [x] = []
takeDiff [] = []

generateTrueCLKEdge :: (Num a) => a -> a -> [a]
generateTrueCLKEdge tstart period = [ tstart + period * fromIntegral i | i <- [0..]]
generateTrueCLKEdge' :: (RealFrac a) => a -> a -> a -> [a]
generateTrueCLKEdge' thr tstart period = [ tstart + period * fromIntegral i | i <- [s..]]
  where s = floor $ (thr - tstart) / period

parseFileArgs (x:y:xs) = (read x, y) : parseFileArgs xs
parseFileArgs [x] = []
parseFileArgs [] = []


