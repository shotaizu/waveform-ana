module TektronixFormat
  where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Conversion
import qualified Data.Binary.Get as BinG
import GHC.Natural
import GHC.Word

data TektronixFormatB = TektronixFormatB { staticFileInfoB :: StaticFileInfoB
                                           , headerB :: HeaderB
                                           , fastFrameB :: FastFrameB
                                           , curveBufferB :: CurveBufferB
                                           , checkSumB :: CheckSumB
                                           } deriving (Show)

data TektronixFormat = TektronixFormat { staticFileInfo :: StaticFileInfo
                                           , header :: Header
                                           , fastFrame :: FastFrame
                                           , curveBuffer :: CurveBuffer Double
                                           , validCheckSum :: Bool
                                           } deriving (Show)

newtype StaticFileInfoB = StaticFileInfoB B.ByteString deriving (Show)
newtype HeaderB         = HeaderB B.ByteString deriving (Show)
newtype FastFrameB      = FastFrameB B.ByteString deriving (Show)
newtype CurveBufferB    = CurveBufferB B.ByteString deriving (Show)
newtype CheckSumB       = CheckSumB B.ByteString deriving (Show)

data StaticFileInfo = StaticFileInfo { byteOrderVerification :: Natural
                                     , versionNumver :: B.ByteString
                                     , nDigitsByte :: Word8
                                     , nFastFrames :: Natural
                                     , sizeWaveformHeader :: Natural
                                     } deriving (Show)
data Header         = Header { setType :: Maybe WaveformType
                             , wfmCnt :: Natural
                             , impDimRefCount :: Natural
                             , expDimRefCount :: Natural
                             , dataType :: Maybe DataType
                             , summaryFrame :: Maybe SummaryFrame
                             , pixMapDisplayFormat :: Maybe PixMapDisplayFormat
                             , expDim1 :: ExpDimHeader
                             , expDim2 :: ExpDimHeader
                             , impDim1 :: ImpDimHeader
                             , impDim2 :: ImpDimHeader
                             , timeBaseInfo1 :: TimeBaseInfo
                             , timeBaseInfo2 :: TimeBaseInfo
                             , realPointOffset :: Natural
                             , ttOffset :: Double
                             , fracSec :: Double
                             , gmtSec :: Int
                             , stateFlags :: Natural
                             , prechareStartOffset :: Natural
                             , dataStartOffset :: Natural
                             , postchargeStartOffset :: Natural
                             , postchargeStopOffset :: Natural
                             , endOfCurveBufferOffset :: Natural
                             } deriving (Show)
data WaveformType = SingleWaveformSet | FastFrameSet deriving (Show, Eq)
data DataType = WfmdataScalarMeas | WfmdataScalarConst | WfmdataVector | WfmdataInvalid | WfmdataWfmdb | WfmdataDigital deriving (Show, Eq)
data SummaryFrame = SummaryFrameOff | SummaryFrameAverage | SummaryFrameEnvelope deriving (Show, Eq)
data PixMapDisplayFormat = DsyFormatInvalid | DsyFormatYT | DsyFormatXY | DsyFormatXYZ deriving (Show, Eq)
data ExpFormat = ExplicitInt16 | ExplicitInt32 | ExplicitUInt32 | ExplicitUInt64 | ExplicitFP32 | ExplicitFP64 | ExplicitInvalidFormat | ExplicitUInt8 | ExplicitInt8 deriving (Show, Eq)
data ExpStorageType = ExplicitSample | ExplicitMinMax | ExplicitVertHist | ExplicitHorHist | ExplicitRowOrder | ExplicitColumnOrder | ExplicitInvalidStorage deriving (Show, Eq)
data Sweep = SweepRoll | SweepSample | SweepET | SweepInvalid deriving (Show, Eq)
data TypeOfBase = BaseTime | BaseSpectralMag | BaseSpectralPhase | BaseInvalid deriving (Show, Eq)
data ExpDimHeader = ExpDimHeader { expDimScale :: Double
                                 , expDimOffset :: Double
                                 , expDimSize :: Natural
                                 , expUnits :: B.ByteString
                                 , expDimResolution :: Double
                                 , expDimRefPoint :: Double
                                 , expFormat :: Maybe ExpFormat
                                 , expStorageType :: Maybe ExpStorageType
                                 } deriving (Show)
data ImpDimHeader = ImpDimHeader { impDimScale :: Double
                                 , impDimOffset :: Double
                                 , impDimSize :: Natural
                                 , impUnits :: B.ByteString
                                 } deriving (Show)
data TimeBaseInfo = TimeBaseInfo { realPointSpacing :: Natural
                                 , sweep :: Maybe Sweep
                                 , typeOfBase :: Maybe TypeOfBase
                                 } deriving (Show)
newtype FastFrame      = FastFrame B.ByteString deriving (Show)
newtype CurveBuffer a  = CurveBuffer [a] deriving (Show)
type Voltage = Double
type Time = Double
type DataPoint = (Time, Voltage)
instance Functor CurveBuffer where
  -- fmap :: ([a] -> [b]) -> CurveBuffer a -> CurveBuffer b
  fmap f (CurveBuffer x) = CurveBuffer (map f x)
instance Applicative CurveBuffer where
  pure f = CurveBuffer [f]
  (CurveBuffer fs) <*> (CurveBuffer xs) = CurveBuffer (fs <*> xs)

parseStaticFileInfo :: StaticFileInfoB -> StaticFileInfo
parseStaticFileInfo (StaticFileInfoB s) = StaticFileInfo {  byteOrderVerification = fromIntegral $ BinG.runGet BinG.getWord16le (cropByteString 0 2 s)
                                                         , versionNumver = cropByteString 2 8 s
                                                         , nDigitsByte = B.index s 10
                                                         , nFastFrames = getNFastFrame (StaticFileInfoB s)
                                                         , sizeWaveformHeader = fromIntegral $ BinG.runGet BinG.getWord16le (cropByteString 76 2 s)
                                                       }

parseHeader :: HeaderB -> Header
parseHeader (HeaderB h) =  Header { setType = getSetType
                                  , wfmCnt = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (82 - headerOffset) 4 h)
                                  , impDimRefCount = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x072 - headerOffset) 4 h)
                                  , expDimRefCount = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x076 - headerOffset) 4 h)
                                  , dataType = getDataType
                                  , summaryFrame = getSummaryFrame
                                  , pixMapDisplayFormat = getPixMapDisplayFormat
                                  , expDim1 = convExpDimHeader (cropByteString (0xa8 - headerOffset) 156 h)
                                  , expDim2 = convExpDimHeader (cropByteString (0x148 - headerOffset) 156 h)
                                  , impDim1 = convImpDimHeader (cropByteString (0x1e8 - headerOffset) 132 h)
                                  , impDim2 = convImpDimHeader (cropByteString (0x270 - headerOffset) 132 h)
                                  , timeBaseInfo1 = convTimeBaseInfo (cropByteString (0x2f8 - headerOffset) 12 h)
                                  , timeBaseInfo2 = convTimeBaseInfo (cropByteString (0x304 - headerOffset) 12 h)
                                  , realPointOffset  = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x310 - headerOffset) 4 h)
                                  , ttOffset = BinG.runGet BinG.getDoublele (cropByteString (0x314 - headerOffset) 8 h)
                                  , fracSec = BinG.runGet BinG.getDoublele (cropByteString (0x31c - headerOffset) 8 h)
                                  , gmtSec = fromIntegral $ BinG.runGet BinG.getInt32le (cropByteString (0x324 - headerOffset) 4 h)
                                  , stateFlags = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x328 - headerOffset) 4 h)
                                  , prechareStartOffset = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x332 - headerOffset) 4 h)
                                  , dataStartOffset = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x336 - headerOffset) 4 h)
                                  , postchargeStartOffset = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x33a - headerOffset) 4 h)
                                  , postchargeStopOffset = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x33e - headerOffset) 4 h)
                                  , endOfCurveBufferOffset = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x342 - headerOffset) 4 h)
                                  }
                                    where headerOffset = 78
                                          st = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (78 - headerOffset) 4 h)
                                          getSetType
                                            | st == 0 = Just SingleWaveformSet
                                            | st == 1 = Just FastFrameSet
                                            | otherwise = Nothing
                                          dt = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x7a - headerOffset) 4 h)
                                          getDataType
                                            | dt == 0 = Just WfmdataScalarMeas
                                            | dt == 1 = Just WfmdataScalarConst
                                            | dt == 2 = Just WfmdataVector
                                            | dt == 4 = Just WfmdataInvalid
                                            | dt == 5 = Just WfmdataWfmdb
                                            | dt == 6 = Just WfmdataDigital
                                            | otherwise = Nothing
                                          sf = fromIntegral $ BinG.runGet BinG.getWord16le (cropByteString (0x9a - headerOffset) 2 h)
                                          getSummaryFrame
                                            | sf == 0 = Just SummaryFrameOff
                                            | sf == 1 = Just SummaryFrameAverage
                                            | sf == 2 = Just SummaryFrameEnvelope
                                            | otherwise = Nothing
                                          pf = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x9c - headerOffset) 4 h)
                                          getPixMapDisplayFormat
                                            | pf == 0 = Just DsyFormatInvalid
                                            | pf == 1 = Just DsyFormatYT
                                            | pf == 2 = Just DsyFormatXY
                                            | pf == 3 = Just DsyFormatXYZ
                                            | otherwise = Nothing

convExpDimHeader :: B.ByteString -> ExpDimHeader
convExpDimHeader bs = ExpDimHeader { expDimScale = BinG.runGet BinG.getDoublele (cropByteString (0x0a8 - hO) 8 bs)
                                   , expDimOffset = BinG.runGet BinG.getDoublele (cropByteString (0x0b0 - hO) 8 bs)
                                   , expDimSize = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x0b8 - hO) 4 bs)
                                   , expUnits = cropByteString (0xbc - hO) 20 bs
                                   , expDimResolution = BinG.runGet BinG.getDoublele (cropByteString (0x0e0 - hO) 8 bs)
                                   , expDimRefPoint = BinG.runGet BinG.getDoublele (cropByteString (0x0e8 - hO) 8 bs)
                                   , expFormat = getExpFormat
                                   , expStorageType = getExpStorageType
                                   }
                                     where hO = 0x0a8
                                           ef = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0xf0 - hO) 4 bs)
                                           getExpFormat 
                                             | ef == 0 = Just ExplicitInt16
                                             | ef == 1 = Just ExplicitInt32
                                             | ef == 2 = Just ExplicitUInt32
                                             | ef == 3 = Just ExplicitUInt64
                                             | ef == 4 = Just ExplicitFP32
                                             | ef == 5 = Just ExplicitFP64
                                             | ef == 6 = Just ExplicitUInt8
                                             | ef == 7 = Just ExplicitInt8
                                             | ef == 8 = Just ExplicitInvalidFormat
                                             | otherwise = Nothing
                                           et = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x0f4 - hO) 4 bs)
                                           getExpStorageType 
                                             | et == 0 = Just ExplicitSample
                                             | et == 1 = Just ExplicitMinMax
                                             | et == 2 = Just ExplicitVertHist
                                             | et == 3 = Just ExplicitHorHist
                                             | et == 4 = Just ExplicitRowOrder
                                             | et == 5 = Just ExplicitColumnOrder
                                             | et == 6 = Just ExplicitInvalidStorage
                                             | otherwise = Nothing

convImpDimHeader :: B.ByteString -> ImpDimHeader
convImpDimHeader bs = ImpDimHeader { impDimScale = BinG.runGet BinG.getDoublele (cropByteString (0x1e8 - hO) 8 bs)
                                   , impDimOffset = BinG.runGet BinG.getDoublele (cropByteString (0x1f0 - hO) 8 bs)
                                   , impDimSize = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x1f8 - hO) 4 bs)
                                   , impUnits = cropByteString (0x1fc - hO) 20 bs
                                   }
                                     where hO = 0x1e8

convTimeBaseInfo :: B.ByteString -> TimeBaseInfo
convTimeBaseInfo bs = TimeBaseInfo { realPointSpacing = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x2f8 - hO) 4 bs)
                                   , sweep = getSweep
                                   , typeOfBase = getTypeOfBase
                                   }
                                     where hO = 0x2f8
                                           sw = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x2fc - hO) 4 bs)
                                           getSweep 
                                             | sw == 0 = Just SweepRoll
                                             | sw == 1 = Just SweepSample
                                             | sw == 2 = Just SweepET
                                             | sw == 3 = Just SweepInvalid
                                             | otherwise = Nothing
                                           tb = fromIntegral $ BinG.runGet BinG.getWord32le (cropByteString (0x300 - hO) 4 bs)
                                           getTypeOfBase
                                             | tb == 0 = Just BaseTime
                                             | tb == 1 = Just BaseSpectralMag
                                             | tb == 2 = Just BaseSpectralPhase
                                             | tb == 3 = Just BaseInvalid
                                             | otherwise = Nothing

parseFastFrame :: FastFrameB -> FastFrame
parseFastFrame (FastFrameB b) = FastFrame b
parseCurveBuffer :: Integral t => BinG.Get t -> CurveBufferB -> CurveBuffer Double
parseCurveBuffer g cb = fmap fromIntegral (convCurveBuffer g cb)

parseTektronixFormat :: TektronixFormatB -> TektronixFormat
parseTektronixFormat f = TektronixFormat { staticFileInfo = (parseStaticFileInfo . staticFileInfoB) f
                                         , header = (parseHeader . headerB) f
                                         , fastFrame  = (parseFastFrame . fastFrameB ) f
                                         , curveBuffer = (parseCurveBuffer BinG.getInt8 . curveBufferB) f
                                         , validCheckSum = validateWFMCheckSum f
                                         }


newtype CheckSum       = CheckSum B.ByteString deriving (Show)

readTektronixFile' :: String -> IO TektronixFormatB
readTektronixFile' path = do
  content <- B.readFile path
  (fi, left) <- (return . splitPart takeStatFileInfo dropStatFileInfo) content
  (head, leftHead) <- (return . splitPart takeHead dropHead) left
  (fastf, leftFastF) <- (return . splitPart (takeFastFrame fi) (dropFastFrame fi)) leftHead
  (curveBuf, checksumByteString) <- (return . splitPart takeCurveBuf dropCurveBuf) leftFastF
  return TektronixFormatB { staticFileInfoB = fi
                           , headerB = head
                           , fastFrameB = fastf
                           , curveBufferB = curveBuf
                           , checkSumB = CheckSumB checksumByteString
                           }

readTektronixFile :: String -> IO TektronixFormat
readTektronixFile path = do
  parseTektronixFormat <$> readTektronixFile' path


nFieldStatinfo = 78
takeStatFileInfo = StaticFileInfoB . fst . B.splitAt nFieldStatinfo
dropStatFileInfo = snd . B.splitAt nFieldStatinfo
nFieldHeader = 0x346 - 0x4e
-- 88 + 100 + 56 + 100 + 56 + 76 + 56 + 76 + 56 + 12 + 12 + 24 + 30 
takeHead = HeaderB . fst . B.splitAt nFieldHeader
dropHead = snd . B.splitAt nFieldHeader
takeFastFrame sf c = FastFrameB $ B.take ((fromIntegral n - 1) * 54) c
  where n = getNFastFrame sf
dropFastFrame sf c = snd $ B.splitAt ((fromIntegral n - 1) * 54) c
  where n = getNFastFrame sf
nFieldCheckSum = 8
takeCurveBuf x = (CurveBufferB . fst . B.splitAt (B.length x - nFieldCheckSum)) x
dropCurveBuf x = (snd . B.splitAt (B.length x - nFieldCheckSum)) x

getNFastFrame :: Integral a => StaticFileInfoB -> a
getNFastFrame (StaticFileInfoB x) = fromIntegral (1 + BinG.runGet BinG.getWord32le (cropByteString 72 4 x))

validateWFMCheckSum :: TektronixFormatB -> Bool
validateWFMCheckSum w = (==) cc (calcWFMCheckSum w)
  where CheckSumB c = checkSumB w
        cc =  BinG.runGet BinG.getWord64le c

splitPart :: (B.ByteString -> a) -> (B.ByteString -> B.ByteString) -> B.ByteString -> (a, B.ByteString)
splitPart f g b = (f b, g b)

cropByteString i n = B.drop i . B.take (i+n)

catWFMWoCheckSum :: TektronixFormatB -> B.ByteString
catWFMWoCheckSum wf = B.concat [s, h, f, c]
  where StaticFileInfoB s = staticFileInfoB wf
        HeaderB h         = headerB wf
        FastFrameB f      = fastFrameB wf
        CurveBufferB c    = curveBufferB wf

calcWFMCheckSum :: TektronixFormatB -> Word64
calcWFMCheckSum = B.foldl addIntegerAB 0 . catWFMWoCheckSum
  where addIntegerAB a b = fromIntegral a + fromIntegral b

convCurveBuffer :: Num t => BinG.Get t -> CurveBufferB -> CurveBuffer t
convCurveBuffer f (CurveBufferB buf) =  CurveBuffer a
  where cnt = 1
        a = map (BinG.runGet f) (splitN cnt buf)

convVoltage :: Double -> Double -> CurveBuffer Double -> CurveBuffer Voltage
convVoltage a b = fmap (\x -> a * x + b) 
convTime :: Double -> Double -> Int -> Double
convTime a b i = a * fromIntegral i + b

buildTimeBuffer :: Double -> Double -> Natural -> Natural -> [Time]
buildTimeBuffer scale offset preStart dataStart = map (\x -> (fromIntegral x - fromIntegral dataStart - fromIntegral preStart) * scale + offset) [0..]

zipTimeCurveBuf :: [Time] -> CurveBuffer Voltage -> CurveBuffer (Time,Voltage)
zipTimeCurveBuf tb (CurveBuffer cb) = CurveBuffer (zip tb cb)

takeTimeVoltageCurve :: TektronixFormat -> CurveBuffer DataPoint
takeTimeVoltageCurve f = zipTimeCurveBuf (buildTimeBuffer tscale toffset preCS dS) (convVoltage vscale voffset (curveBuffer f))
  where h = header f
        eDH = expDim1 h
        iDH = impDim1 h
        tscale = impDimScale iDH
        toffset = impDimOffset iDH
        vscale = expDimScale eDH
        voffset = expDimOffset eDH
        preCS = prechareStartOffset h
        dS = dataStartOffset h
        


byteOffsetHeaderB = 0x04e
getExpDimScale1 :: HeaderB -> Double
getExpDimScale1 (HeaderB h) = BinG.runGet BinG.getDoublele (cropByteString (0x0a8 - byteOffsetHeaderB) 8 h)
getExpDimScale2 :: HeaderB -> Double
getExpDimScale2 (HeaderB h) = BinG.runGet BinG.getDoublele (cropByteString (0x148 - byteOffsetHeaderB) 8 h)
getExpDimOffset1 :: HeaderB -> Double
getExpDimOffset1 (HeaderB h) = BinG.runGet BinG.getDoublele (cropByteString (0x0b0 - byteOffsetHeaderB) 8 h)
getExpDimOffset2 :: HeaderB -> Double
getExpDimOffset2 (HeaderB h) = BinG.runGet BinG.getDoublele (cropByteString (0x150 - byteOffsetHeaderB) 8 h)

splitN cnt bs
  | B.length bs < cnt = []
  | otherwise = B.take cnt bs : (splitN cnt . B.drop cnt) bs

findCrossPointLinear :: Voltage -> DataPoint -> DataPoint -> [DataPoint]
findCrossPointLinear thr (x0,y0) (x1,y1)
  | y0 > y1 = []
  | thr < y0 || thr > y1 = []
  | y1 == y0 && y1 == thr = [((x1 - x0) / 2.0,thr)]
  | otherwise = [ (x0 + (thr - y0) * (x1 - x0) / (y1 - y0), thr)]

findCrossPoints :: Voltage -> CurveBuffer DataPoint -> [DataPoint]
findCrossPoints thr (CurveBuffer (x:xx:xs)) = findCrossPointLinear thr x xx ++ findCrossPoints thr (CurveBuffer (xx:xs))
findCrossPoints thr (CurveBuffer [x]) = []
findCrossPoints thr (CurveBuffer []) = []

flatInTime = map fst

findCenterEdge :: (Ord a, Real a) => [a] -> a
findCenterEdge = minimum . filter (0 <=) 
compCenterEdge :: [Double] -> [Double] -> Double
compCenterEdge d1 d2 = fst $ foldl (\(x,y) (xx,yy) -> if y < yy then (x,y) else (xx,yy)) (9999.9, 9999.9) [(x, ax) | x <- map (\q -> q - findCenterEdge d1) d2, let ax = abs x]

