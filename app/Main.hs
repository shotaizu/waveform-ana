module Main where
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import GHC.Natural
import qualified Statistics.Sample as S
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import TektronixFormat

data PrgMode = Trigger | Period | TIE | Diff | Waveform
  deriving (Show, Eq)
data TIEMode = TIELocal | TIEGlobal
  deriving (Show, Eq)
data Options = Options { optVerbose :: Bool
                       , optMode :: PrgMode
                       , optStartEpochTime :: Int
                       , optIdealPeriod :: Double
                       , optTIEGL :: TIEMode
                       , optDiffMin :: Double
                       , optDiffMax :: Double
                       } deriving (Show)

startOptions = Options { optVerbose = False
                       , optMode  = Period
                       , optStartEpochTime = 1614135600 -- 2021-02-24 12:00:00
                       , optIdealPeriod = 0
                       , optTIEGL = TIELocal
                       , optDiffMin = 100.0
                       , optDiffMax = -100.0
                       }

basicUsage prgname = 
  prgname ++ ": tools to analyze jitter with Tektronix oscilloscope\n\n"
  ++ "Usage for trigger-mode: \n"
  ++ "\t$ " ++ prgname ++ " -t {threshold1} {data1} {threshold2} {data2}\n"
  ++ "Usage for period-mode: \n"
  ++ "\t$ " ++ prgname ++ " -p {threshold} {data}\n"
  ++ "Usage for tie-mode: \n"
  ++ "\t$ " ++ prgname ++ " --tie {threshold} {data}\n" -- [threshold2] [data2]\n"
  ++ "Usage for diff-mode: \n"
  ++ "\t$ " ++ prgname ++ " -d {threshold} {data} {threshold2} {data2}\n"
  ++ "\t\tIt outputs \"{clock edge time} {difference of clock edge}\" to stdout\n"

options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "v" ["verbose"] (NoArg (\opt -> return opt {optVerbose = True})) "Enable verbose message"
          , Option "t" ["trigger"] (NoArg (\opt -> return opt {optMode = Trigger})) "Analysis mode: Trigger -> relation of 2 channels"
          , Option "p" ["period"] (NoArg (\opt -> return opt {optMode = Period})) "Analysis mode: Period -> just measure 1 channel and calculate its period"
          , Option "" ["tie"] (NoArg (\opt -> return opt {optMode = TIE})) "Analysis mode: TIE -> calculate difference from ideal clcok edge"
          , Option "d" ["diff"] (NoArg (\opt -> return opt {optMode = Diff})) "Analysis mode: Difference -> calculate difference of edge time between two clock input"
          , Option "w" ["waveform"] (NoArg (\opt -> return opt {optMode = Waveform})) "Analysis mode: Waveform -> dump waveform"
          , Option "" ["tstart"]
            (ReqArg 
              (\arg opt -> return opt { optStartEpochTime = read arg, optTIEGL = TIEGlobal }) "TIME")
            "For TIE analysis: base time in epoch format as global"
          , Option "" ["idealperiod"]
            (ReqArg 
              (\arg opt -> return opt { optIdealPeriod = read arg}) "TIME")
            "For TIE analysis: ideal period in sec"
          , Option "" ["localtie"] (NoArg (\opt -> return opt {optTIEGL = TIELocal})) "TIE ideal edge origin : set Local"
          , Option "" ["diff_min"] (ReqArg (\arg opt -> return opt {optDiffMin = read arg}) "TIME") "Difference mode: lower limit to find same clock edge in sec (default = -period/2.0)"
          , Option "" ["diff_max"] (ReqArg (\arg opt -> return opt {optDiffMax = read arg}) "TIME") "Difference mode: higher limit to find same clock edge in sec (default = period/2.0)"
          , Option "h" ["help"]
            (NoArg
              (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (basicUsage prg)
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
              , optDiffMin = diffMin
              , optDiffMax = diffMax
              } = opts
      files = parseFileArgs nonoptArg :: [(Double, String)]
  when verbose $ hPrint stderr opts
  when verbose $ hPutStrLn stderr $ (concatMap (\(x, y) -> "threshold: " ++ show x ++ ", file: " ++ show y ++ "/ ") . take 2) files

  flagFiles <- mapM doesFileExist ((snd . unzip) (take 2 files))
  when (not (and flagFiles)) $ do
    hPutStrLn stderr $ "Could not read files: " ++ show flagFiles
    exitFailure
  tekFiles <- mapM readTektronixFile ((snd . unzip ) files)
  let
    thrWithTekFiles = zipWith (\x y -> (fst x, y)) files tekFiles
    xpoints = map findXPoints thrWithTekFiles
  case mode of 
    Trigger -> do
      if length files < 2
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        print $ compCenterEdge (xpoints !! 0) (xpoints !! 1)
        exitSuccess
    Period -> do
      if null files
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        mapM_ print $ (takeDiff . head) xpoints
        exitSuccess
    TIE -> do
      if null files
      then do
        hPutStrLn stderr (usageInfo (basicUsage progname) options)
        exitFailure
      else do
        let
          (thr, f) = head thrWithTekFiles
          sec = (gmtSec . header) f - tstart
          diffEpochTime = if tieMode == TIEGlobal then fromIntegral sec  + (fracSec . header) f else 0
          measEdge  = (flatInTime . findCrossPoints thr . takeTimeVoltageCurve) f
          usedPeriod = if idealPeriod == 0 then (S.mean . V.fromList . takeDiff) measEdge else idealPeriod
          idealEdge = generateTrueCLKEdge' (diffEpochTime + (impDimOffset . impDim1 . header) f) 0 usedPeriod
          delta = ((head . takeDiff . take 2 ) idealEdge - usedPeriod) / usedPeriod
          epsilon = 1e-3
        when verbose $ hPutStrLn stderr $ "header GMT time = " ++ (show . gmtSec . header) f
        when (epsilon < delta) $ hPutStrLn stderr $ "Warning: neumerical digits-loss in grobal clock edge calculation: " ++ show delta
        mapM_ printDP $ zipWith (\x y -> (x, x-y)) (map (+ diffEpochTime) measEdge)  idealEdge
        exitSuccess
    Diff -> do
      if length files < 2
        then do
          hPutStrLn stderr (usageInfo (basicUsage progname) options)
          exitFailure
        else do
          let
            period = (S.mean . V.fromList . takeDiff) (xpoints !! 0)
            searchMin = if diffMin > diffMax then (- period / 2.0) else diffMin
            searchMax = if diffMin > diffMax then    period / 2.0  else diffMax
          mapM_ (printDP . (\(x,y)-> (x, x-y))) $ catMatchedPoints searchMin searchMax (xpoints !! 0) (xpoints !! 1)
          exitSuccess
    Waveform -> do
      if null files
        then do
          hPutStrLn stderr (usageInfo (basicUsage progname) options)
          exitFailure
        else do
          let CurveBuffer cb = (takeTimeVoltageCurve . head) tekFiles
          mapM_ printDP cb
          exitSuccess
         

  
printDP (x,y) = putStrLn $ show x ++ " " ++ show y

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

findXPoints (thr, f) = flatInTime $ findCrossPoints thr (takeTimeVoltageCurve f)


catMatchedPoints :: (Num a, Ord a) => a -> a -> [a] -> [a] -> [(a,a)]
catMatchedPoints ymin ymax (x:xs) ys
  | null matched = catMatchedPoints ymin ymax xs ys
  | otherwise = (x,head matched) : catMatchedPoints ymin ymax xs (tail matched)
  where matched = findFirstMatchedPoint ymin ymax x ys
catMatchedPoints ymin ymax [x] ys
  | null matched = []
  | otherwise = [(x,head matched)]
  where matched = findFirstMatchedPoint ymin ymax x ys
catMatchedPoints _ _ _ [] = []
catMatchedPoints _ _ [] _ = []

findFirstMatchedPoint :: (Num a, Ord a) => a -> a -> a -> [a] -> [a]
findFirstMatchedPoint xmin xmax x (y:ys)
  | (y < x + xmax) && (y > x + xmin) = y:ys
  | otherwise = findFirstMatchedPoint xmin xmax x ys
findFirstMatchedPoint _ _ x [] = []



