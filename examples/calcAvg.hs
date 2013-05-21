import System.Cmd
import System.IO
import System.Environment
import Data.List

main :: IO ()
main = do name   <- fmap head getArgs
          iter'   <- fmap (flip (!!) 1) getArgs
          let iter = 3 --read iter' :: Int
          powers' <- fmap (flip (!!) 2) getArgs 
          let powers = 1 --read powers' :: Int
          file <- readFile name
          putStrLn $ show $ stringToAvg file iter powers

stringToAvg :: String -> Int -> Int -> [Double]
stringToAvg s iter powers = let allPowers = take iter $ map (take powers) $ splitEvery 2 (words s) :: [[String]]
                                allTimes = map head $ allPowers :: [String]
                                allAvgs = avg (map read allTimes :: [Double])
                            in [allAvgs]




avg xs = (foldl1' (+) xs) / (fromIntegral (length xs))

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list
