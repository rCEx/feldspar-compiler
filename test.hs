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
dotProd xs ys = parFold (+) $ zipWith (*) xs ys

vecMul :: Vector1 Index -> Vector1 Index -> Vector1 Index
vecMul = zipWith (*)


--fftInt :: Data Index -> Vector1 Index -> Vector1 Index
--fftInt n xs = fftCore n (+) xs

parFold :: (Syntax a, Num a) => (a -> a -> a) -> Vector a -> a
parFold f xs = head $ forLoop (log2 $ length xs-1) xs $ \i' acc -> let i = i' + 1 in indexed (length acc) $ \j -> condition 
                                                                                          (j `mod` (2^i) == 0)
                                                                                          (f (acc ! j) 
                                                                                             (acc ! (j+(2^(i-1)))))
                                                                                          0 -- doesn't matter.

foldTest :: Vector1 Index -> Data Index 
foldTest xs = parFold (+) xs

