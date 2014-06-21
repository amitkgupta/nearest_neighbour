{-# LANGUAGE BangPatterns #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

type Label = Field

type Feature = Integer

data Observation = Observation { label :: !Label
                               , features :: !(V.Vector Feature)
                               } deriving (Show, Eq)

instance FromRecord Observation where
  parseRecord !v = do
    V.mapM parseField (V.tail v) >>=
      return . Observation (V.head v)

parseRecords :: BS.ByteString -> Either String (V.Vector Observation)
parseRecords = decode HasHeader

dist :: (V.Vector Feature) -> (V.Vector Feature) -> Integer
dist !x !y = V.sum $ V.map (^2) $ V.zipWith (-) x y

closerTo :: (V.Vector Feature) -> Observation -> Observation -> Ordering
closerTo !target !o1 !o2 = compare (dist target (features o1)) (dist target (features o2))

classify :: (V.Vector Observation) -> (V.Vector Feature) -> Label
classify !os !fs = label where
  (Observation label _) = V.minimumBy (closerTo fs) os

checkCorrect :: (V.Vector Observation) -> Observation -> Int
checkCorrect !training (Observation label features)
  | label == classify training features = 1
  | otherwise = 0

main = do
  validationSample <- BS.readFile "validationsample.csv"
  trainingSample <- BS.readFile "trainingsample.csv"

  let Right validation = parseRecords validationSample
  let Right training = parseRecords trainingSample
  
  print (V.sum (V.map (checkCorrect training) validation))
