module ParScan where
import qualified Prelude
import Feldspar
import Feldspar.Vector
import Feldspar.Compiler



type Level    = Data Index
type Position = Data Index

leftIx :: Level -> Position -> Position
leftIx l p = ((p .>>. l) .<<. l) - 1

cond :: Level -> Position -> Data Bool
cond l p = testBit (p .>>. l) 0

step :: Syntax a => (a -> a -> a) -> Level -> Vector a -> Vector a
step f l as = indexed (length as) $ \i ->
    cond l i
        ? f (as ! leftIx l i) (as!i)
        $ (as!i)

log2 :: Data Length -> Data Length
log2 a = bitSize a - bitScan a

sklansky :: Syntax a => (a -> a -> a) -> Vector a -> Vector a
sklansky f a = forLoop (log2 (length a)) a (step f)

sklansky2 :: Syntax a => WordN -> (a -> a -> a) -> Vector a -> Vector a
sklansky2 0 f = id
sklansky2 n f = force . step f (value (n-1)) . sklansky2 (n-1) f



testProg :: Data Length -> Vector1 Index
testProg l = sklansky (+) (0 ... (l-1))

test = eval testProg 16

testProg2 :: Length -> Vector1 Index
testProg2 l = sklansky2 (eval log2 l) (+) (0 ... (value l - 1))

test2 = eval (testProg2 16)

