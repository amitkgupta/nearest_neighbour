{-# LANGUAGE BangPatterns #-}

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

data Observation = Observation
    { label    :: !Label
    , features :: !Features
    } deriving (Show, Eq)

type Label = CSV.Field
type Feature = Integer
type Features = V.Vector Feature
type Observations = V.Vector Observation

main = do
    validationSample <- fmap parseRecords $ BL.readFile "validationsample.csv"
    trainingSample   <- fmap parseRecords $ BL.readFile "trainingsample.csv"

    case (validationSample, trainingSample) of
        (Right v, Right t) -> runClassifier v t
        _otherwise         -> putStrLn "Parsing error"

runClassifier :: Observations -> Observations -> IO ()
runClassifier validation training =
    let n = V.length validation
        results = V.map (classify training) validation
        score l o = if l == label o then 1 else 0
        correct = V.zipWith score results validation
     in print (fromIntegral (V.sum correct) / fromIntegral n)

dist :: Observation -> Observation -> Integer
dist o1 o2 = V.sum $ V.map (^2) $ V.zipWith (-) f1 f2 where
    (f1, f2) = (features o1, features o2)

closestTo :: Observation -> Observation -> Observation -> Ordering
closestTo target o1 o2 = compare (dist target o1) (dist target o2)

classify :: Observations -> Observation -> Label
classify training obs = label closest where
    closest = V.minimumBy (closestTo obs) training

parseRecords :: BL.ByteString -> Either String Observations
parseRecords = CSV.decode CSV.HasHeader

instance CSV.FromRecord Observation where
    parseRecord v = do
        pixels <- V.mapM CSV.parseField (V.tail v)
        return $ Observation (V.head v) pixels
