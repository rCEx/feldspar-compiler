import qualified Prelude as P

import Feldspar
import Feldspar.Vector
--import Feldspar.Repa
import ParScan
import Bitonic

--import FFT.Pull

import Feldspar.Compiler
import Feldspar.Compiler.Imperative.FromCore

import Debug.Trace

{- dotProd returns a vector and not a scalar. We are really only
 - interested in the first element. Why could prepend a 'head' call, but taking
 - the first element of a for-loop doesn't play nice with PIRE currently.  
 -}
dotProd :: Vector1 Word64 -> Vector1 Word64 -> Vector1 Word64
dotProd xs ys = parFold (+) $ zipWith (*) xs ys

dp' :: Vector1 Word64 -> Vector1 Word64 -> Data Word64
dp' a b = sum $ zipWith (*) a b

parFold :: (Syntax a, Num a) => (a -> a -> a) -> Vector a -> Vector a
parFold f xs = forLoop (log2 (length xs) - 1) xs $ \i' acc -> let i = i' + 1 in indexed (length acc) $ \j -> condition 
                                                                                          (j `mod` (2^i) == 0)
                                                                                          (f (acc ! j) 
                                                                                             (acc ! (j+(2^(i-1)))))
                                                                                          0 -- doesn't matter.

parScan :: Vector1 Index -> Vector1 Index
parScan xs = sklansky (+) xs


bitonicTest :: Data Index -> Vector1 Index -> Vector1 Index
bitonicTest n xs = tsort n xs
