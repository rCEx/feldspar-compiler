import System.Cmd

main :: IO ()
main = sequence_ $ map system $ runAll "./a.out " 24

runAll name n = map ((++) name . show) (sizes n)

sizes :: Integer -> [Integer]
sizes n = [2^i | i <- [0..n]]
