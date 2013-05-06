import qualified Prelude as P

import Feldspar
import Feldspar.Vector
--import Feldspar.Repa
import ParScan

import Feldspar.Compiler
import Feldspar.Compiler.Imperative.FromCore


test :: Data [Index]
test = parallel 10 (+3)

test2 :: Data Index -> Data Index -> Data Index
test2 x y = x+y

test3 :: Vector1 Index -> Vector1 Index -> Vector1 Index
test3 = zipWith (*)

dotProd :: Vector1 Index -> Vector1 Index -> Data Index
dotProd xs ys = last $ sklansky (+) $ zipWith (*) xs ys

vecMul :: Vector1 Index -> Vector1 Index -> Vector1 Index
vecMul = zipWith (*)

