{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FFT.Pull where


import qualified Prelude

import Feldspar -- hiding (trace)
import Feldspar.Vector
--import Feldspar.Plugin

import FFT.Utils

type Stage = Index


--fft :: Vector1 (Complex Double) -> Vector1 (Complex Double)
--fft v = bitRev steps $ fftCore steps (\s i -> tws ! s ! i) v
--  where
--    tws   = indexed steps $ twids (length v)
--    steps = ilog2 (length v) - 1

fftCore :: (Type a, Numeric a)
        => Data Index
        -> (Data Stage -> Data Index -> Data a)
        -> Vector1 a
        -> Vector1 a
fftCore n twf = composeOn (stage twf) (reverse (0...n))

dft2 :: Numeric a => Data a -> (Data a,Data a) -> (Data a,Data a)
dft2 w (x0,x1) = (x0+x1,(x0-x1)*w)

stage :: (Numeric a)
      => (Data Stage -> Data Index -> Data a)
      -> Data Stage -> Vector1 a -> Vector1 a
stage twf s v = indexed (length v) ixf
  where
    ixf i = condition (tstBit i s) y1 y0
      where
        (y0,y1) = dft2 (twf s i) (a,b)
        a    = v ! i
        b    = v ! (i `xor` bit s)


twif :: Data Length -> Vector1 Index
twif steps = undefined

--twif :: Data Length -> Vector1 (Complex Double)
--twif steps = twids (1 .<<. steps) steps
--
--twids :: Data Length -> Data Index -> Vector1 (Complex Double)
--twids len s = indexed len $ \i -> cis (-tau * i2f (lsbs s i) / i2f len)
--  where tau = 2 * pi :: Data Double

bitRev :: Type a => Data Index -> Vector1 a -> Vector1 a
bitRev n = composeOn riffle (1...n)

riffle :: Syntax a => Data Index -> Vector a -> Vector a
riffle k = permute (const $ rotBit k)

iZero :: Bits a => Data Index -> Data a -> Data a
iZero b w = w + (w .&. complement (oneBits b))

iOne :: Bits a => Data Index -> Data a -> Data a
iOne b w = bit b `xor` iZero b w

--loadFun 'fft

