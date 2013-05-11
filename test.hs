import qualified Prelude as P

import Feldspar
import Feldspar.Vector
--import Feldspar.Repa
import ParScan

--import FFT.Pull

import Feldspar.Compiler
import Feldspar.Compiler.Imperative.FromCore

import Debug.Trace


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


--fftInt :: Data Index -> Vector1 Index -> Vector1 Index
--fftInt n xs = fftCore n (+) xs






seqFold :: Syntax a => (a -> a -> a) -> a -> Vector a -> a
seqFold f init xs = forLoop (length xs) init $ \i acc -> f acc (xs ! i)




parFold :: Syntax a => (a -> a -> a) -> Vector a -> Vector a
parFold f xs = forLoop ((log2 $ length xs)) xs $ \i' acc -> indexed (length acc) $ \j -> condition 
                                                                                          (j `mod` (2^i') == 0)
                                                                                          (f (acc ! j) 
                                                                                             (acc ! (j+(2^(i'-1)))))
                                                                                          (acc ! j)


foldTest :: Vector1 Index -> Vector1 Index
foldTest xs = parFold (+) xs

