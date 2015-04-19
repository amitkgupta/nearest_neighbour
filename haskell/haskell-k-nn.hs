import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Control.Parallel.Strategies as P
import qualified Data.Traversable as T
import Data.Maybe (fromMaybe)

data Observation = Observation
    { label    :: Label
    , features :: Features
    } deriving (Show, Eq)

type Label = BS.ByteString
type Feature = Int
type Features = U.Vector Feature
type Observations = V.Vector Observation

main = do
    validation <- fmap parseFile $ BS.readFile "validationsample.csv"
    training   <- fmap parseFile $ BS.readFile "trainingsample.csv"
    let n = V.length validation
        results = inParallel $ V.map (classify training) validation
        score l o = if l == label o then 1 else 0
        correct = V.zipWith score results validation
     in print (fromIntegral (V.sum correct) / fromIntegral n)

classify :: Observations -> Observation -> Label
classify training obs = label closest where
    closest = V.minimumBy (closestTo obs) training
    closestTo target o1 o2 = compare (dist target o1) (dist target o2)
    dist o1 o2 = vdist (features o1) (features o2)
    vdist v1 v2 = U.sum $ U.map (^2) $ U.zipWith (-) v1 v2

parseFile :: BS.ByteString -> Observations
parseFile = V.fromList . observationsOf . wordsOf . linesOf where
    linesOf = filter (not . BS.null) . drop 1 . C8.split '\n'
    wordsOf = map (C8.split ',')
    observationsOf = inParallel . map lineToObservation
    lineToObservation (l:fs) = Observation l (toFeatures fs)
    toFeatures = U.fromList . map (fromMaybe 0 . fmap fst) . map C8.readInt

inParallel :: T.Traversable t => t a -> t a
inParallel = P.withStrategy (P.parTraversable P.rpar)
