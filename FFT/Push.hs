{-# LANGUAGE TemplateHaskell #-}


module FFT.Push where


import qualified Prelude as P

import Feldspar
import qualified Feldspar.Vector as V
import Feldspar.Vector.Push
import Feldspar.Compiler
import Feldspar.Plugin
import qualified Data.Bits
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Modifiers
import qualified RosettaFFT

import FFT.Utils

-- | DFT2 for Decimation-In-Frequency
dft2 :: Num a => a -> a -> a -> (a,a)
dft2 w x0 x1 = (x0+x1, (x0-x1)*w)

butterfly :: (Syntax a, Num a) => V.Vector a -> V.Vector a -> PushVector a
butterfly ws vs = unhalve $ V.zipWith3 dft2 ws ys zs
  where
    (ys,zs) = halve vs

-- | Cooley-Tukey Radix-2 Decimation In Frequency Fast Fourier Transfrom
--
-- >>> eval (fft (twids 8)) [1,1,1,1,0,0,0,0]
-- [4.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,0.0 :+ 0.0,1.0 :+ (-2.414213562373095),0.9999999999999999 :+ 0.4142135623730949,1.0 :+ (-0.4142135623730949),0.9999999999999997 :+ 2.414213562373095]
--
fft :: (Syntax a, Num a) => V.Vector a -> V.Vector a -> V.Vector a
fft ws vs = forLoop (ilog2 $ V.length vs) vs stage
  where
    stage s xs = freezeToVector
               $ chnk (V.length xs .>>. s) (butterfly (ixmap (.<<. s) ws)) xs

twid :: Data Length -> Data Index -> Data (Complex Double)
twid n k = cis (-2 * pi * i2f k / i2f n)

twids :: Data Length -> V.Vector1 (Complex Double)
twids n = V.indexed (n `div` 2) $ twid n






bitRev :: Type a => V.Vector1 a -> V.Vector1 a
bitRev xs = composeOn (ixmap . rotBit) (1 V.... n) xs
  where n = ilog2 (V.length xs) - 1




-- | Utilities that should go into Feldspar.Core.Frontend.Bits

tstBit :: Bits a => Data a -> Data Index -> Data Bool
tstBit w b = w .&. bit b /= 0

setbit :: Bits a => Data a -> Data Index -> Data a
setbit w b = w .|. bit b

clrbit :: Bits a => Data a -> Data Index -> Data a
clrbit w b = w .&. complement (bit b)

iZero :: Bits a => Data Index -> Data a -> Data a
iZero b w = w + (w .&. complement (oneBits b))

iOne :: Bits a => Data Index -> Data a -> Data a
iOne b w = bit b `xor` iZero b w





-- | QuickCheck

fft' xs = eval (bitRev . fft (twids len)) xs
  where len = value $ P.fromIntegral $ P.length xs

prop_is_fft f g = forAll (suchThat arbitrary (btw 2 10)) $ \l ->
    let len = Data.Bits.shiftL 1 (P.fromIntegral (l::Word))
    in forAll (vectorOf len arbitrary) $ \xs ->
      P.and $ P.zipWith (eqComplex 1e-4) (f xs) (g xs)

eqComplex tol (a:+ai) (b:+bi) = P.and [ tol P.> P.abs (a - b)
                                      , tol P.> P.abs (ai - bi)
                                      ]

btw :: P.Ord a => a -> a -> a -> Bool
btw l h a = a P.> l P.&& a P.< h



-- | Utilities that should go into Feldspar.Vector.Push

chnk :: (Pushy arr1, Syntax b)
      => Data Length            -- ^ Size of the chunks
      -> (V.Vector a -> arr1 b) -- ^ Applied to every chunk
      -> V.Vector a
      -> PushVector b
chnk c f v = Push loop (noc * c)
  where l = V.length v
        noc = l `div` c
        loop func = forM noc $ \i ->
                      do let (Push k _) = toPush $ f (V.take c (V.drop (c*i) v))
                         k (\j a -> func (c*i + j) a)

unpairWith :: (Pushy arr, Syntax a)
           => ((Data Index -> a -> M ()) -> Data Index -> (a,a) -> M ())
           -> arr (a,a) -> PushVector a
unpairWith spread arr = Push (f . spread) (2*l)
  where
    Push f l = toPush arr

-- unpair = unpairWith everyOther

-- everyOther = stride 2 1

unhalve :: (Pushy arr, Len arr, Syntax a)
        => arr (a,a) -> PushVector a
unhalve xs = unpairWith (stride 1 (length xs)) xs

stride :: Data Length -> Data Length
       -> (Data Index -> a -> M b)
       -> Data Index -> (a,a) -> M b
stride n k f ix (a1,a2) = f (n*ix) a1 >> f (n*ix+k) a2




ex v = bitRev $ fft (twids $ V.length v) v

loadFun 'ex

