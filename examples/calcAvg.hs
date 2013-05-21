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
          putStrLn $ show $ stringToAvg file iter

stringToAvg :: String -> Int -> [(Double, Double)]
stringToAvg s iter = let allPowers = map (map read) (splitEvery 2 (words s)) :: [[Double]]
                         allTimes = map head allPowers
                         allAvgs  = map avg $ splitEvery iter allTimes
                         

                         
                     in zip allAvgs (map head $ drop 1 allPowers)




avg xs = (foldl1' (+) xs) / (fromIntegral (length xs))

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list
