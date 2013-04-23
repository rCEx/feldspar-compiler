import qualified Prelude as P

import Feldspar
import Feldspar.Vector

import Feldspar.Compiler
import Feldspar.Compiler.Imperative.FromCore


test :: Data [Index]
test = parallel 10 (+3)

test2 :: Data Index -> Data Index -> Data Index
test2 x y = x+y

test3 :: Vector1 Index -> Vector1 Index -> Vector1 Index
test3 = zipWith (*)

dotProd :: Vector1 Index -> Vector1 Index -> Data Index
dotProd xs ys = sum $ zipWith (*) xs ys


testFold :: Vector1 Index -> Vector1 Index -> Data Index
testFold xs ys = fold (+) 0 $ xs'
  where xs' = force $ zipWith (*) xs ys

testScan :: Vector1 Index -> Vector1 Index
testScan xs = scan (+) 0 xs

