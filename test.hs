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
dotProd xs ys = fold (+) 0 xs'
  where xs' = force $ zipWith (*) xs ys


vecMul :: Vector1 Index -> Vector1 Index -> Vector1 Index
vecMul = zipWith (*)

testScan :: Vector1 Index -> Vector1 Index
testScan = scan (+) 0


matVec :: Vector2 Index -> Vector1 Index -> Vector1 Index
matVec xxs ys = map (dotProd ys) xxs
  --indexed 1 (const $ sum $ zipWith (*) xs ys)
  --where xs = head xxs

pscan :: Vector1 Index -> Vector1 Index
pscan xs = xs'
  where
    f   = (+)
    (l,r) = splitAt (length xs) xs
    xs'   = map (uncurry f) $ zip l r
