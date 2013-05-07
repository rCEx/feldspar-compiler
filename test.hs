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



seqFold :: Syntax a => (a -> a -> a) -> a -> Vector a -> a
seqFold f init xs = forLoop (length xs) init $ \i acc -> f acc (xs ! i)

parFold :: Syntax a => (a -> a -> a) -> a -> Vector a -> Vector a
parFold f init xs = forLoop (log2 (length xs)) xs $ \i acc -> indexed (length acc) $ \j -> acc ! j
--forLoop (log2 (length xs)) init $ \i acc -> step' f i xs
--
--
--step' :: Syntax a => (a -> a -> a) -> Level -> Vector a -> a
--step' f l as = head $ indexed (length as) $ \i ->
--     cond l i
--         ? f (as ! leftIx l i) (as!i)
--         $ (as!i)
--


foldTest :: Vector1 Index -> Vector1 Index
foldTest xs = parFold (+) 0 xs



