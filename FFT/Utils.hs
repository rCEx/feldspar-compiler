module FFT.Utils where

import qualified Prelude as P

import Feldspar
import Feldspar.Vector

composeOn :: Syntax a => (b -> a -> a) -> Vector b -> a -> a
composeOn = flip . fold . flip

rotBit :: Data Index -> Data Index -> Data Index
rotBit 0 _ = P.error "rotBit: k should be at least 1"
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
