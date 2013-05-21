import System.Cmd
import System.IO
import Data.List

main :: IO ()
main = do system compile
          sequence_ $ map system $ runAll outName 23


runAll name n = map ((++) (name ++ " ") . show) (sizes n)

outName              = "-o a.out"
compiler             = "gcc"
feldsparCFiles = "../../../../C_new/"
includePaths         = concat $ intersperse "" $ map ((++) "-I ") ["/usr/local/cuda/include/", feldsparCFiles]
feldsparc99          = feldsparCFiles ++ "feldspar_c99.c"
files                = concat $ intersperse " " [feldsparc99, "main.c"]
options              = concat ["-std=c99"]
libs                 = concat $ intersperse " " $ map ((++) "-l")["OpenCL","m", "rt"]
special              = concat $ intersperse " " $ ["-D_POSIX_C_SOURCE=199309"]

compile = compiler <++> outName <++> options <++> includePaths <++> libs <++> files <++> special

sizes :: Integer -> [Integer]
sizes n = [2^i | i <- [1..n]] -- 2^0 is excluded on purpose


a <++> b = a ++ " " ++ b
