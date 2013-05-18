{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Bitonic where

import qualified Prelude
import Feldspar
import Feldspar.Vector

import qualified Data.List
import Test.QuickCheck hiding ((.&.))

pipeline :: Syntax a => [a -> a] -> a -> a
pipeline []     x = x
pipeline (f:fs) x = share (f x) $ pipeline fs

flipLSBsTo :: Bits a => Data Index -> Data a -> Data a
flipLSBsTo i = (`xor` oneBits (i+1))

vee :: Syntax a
    => (a -> a -> a)
    -> (a -> a -> a)
    -> Data Index -> Vector a -> Vector a
vee f g s v = indexed (length v) ixf
  where
    ixf i = condition (testBit i s) (g a b) (f a b)
      where
        a = v ! i
        b = v ! flipLSBsTo s i

dee :: Syntax a
    => (a -> a -> a)
    -> (a -> a -> a)
    -> Data Index -> Vector a -> Vector a
dee f g s v = indexed (length v) ixf
  where
    ixf i = condition (testBit i s) (g a b) (f a b)
      where
        a = v ! i
        b = v ! (i `xor` bit s)

bitonicMerge :: (Type a, Ord a) => Index -> Vector1 a -> Vector1 a
bitonicMerge n = pipeline [dee max min $ value (n-i) | i <- [1..n]]

tmerge :: (Type a, Ord a) => Data Index -> Vector1 a -> Vector1 a
tmerge n v = share (vee min max (n-1) v) $ \w -> forLoop (n-1) w $ \i -> dee min max (n-(i+2))

tsort :: (Type a, Ord a) => Data Index -> Vector1 a -> Vector1 a
tsort n v = forLoop n v $ \i w -> tmerge (i+1) w





-- * Testing
prop_sorts :: Property
prop_sorts = forAll (suchThat arbitrary (btw 2 10)) $ \(!l) ->
    forAll (vectorOf (2 Prelude.^ l) arbitrary) $ \(!xs) ->
      eval (tsort (value l)) (xs::[Index]) Prelude.== Data.List.sort xs

btw :: Prelude.Ord a => a -> a -> a -> Bool
btw l h a = a Prelude.>= l Prelude.&& a Prelude.< h




composeOn :: Syntax a => (b -> a -> a) -> Vector b -> a -> a
composeOn = flip . fold . flip

rotBit :: Data Index -> Data Index -> Data Index
rotBit 0 _ = Prelude.error "rotBit: k should be at least 1"
rotBit k i = lefts .|. rights
  where
    ir = i .>>. 1
    rights = ir .&. oneBits k
    lefts  = (((ir .>>. k) .<<. 1) .|. (i .&. 1)) .<<. k

tstBit :: Bits a => Data a -> Data Index -> Data Bool
tstBit w b = w .&. bit b /= 0

setbit :: Bits a => Data a -> Data Index -> Data a
setbit w b = w .|. bit b

clrbit :: Bits a => Data a -> Data Index -> Data a
clrbit w b = w .&. complement (bit b)


-- | Set all bits to one
allOnes :: Bits a => Data a
allOnes = complement 0

-- | Set the `n` lowest bits to one
oneBits :: Bits a => Data Index -> Data a
oneBits n = complement (allOnes .<<. n)

-- | Extract the `k` lowest bits
lsbs :: Bits a => Data Index -> Data a -> Data a
lsbs k i = i .&. oneBits k
