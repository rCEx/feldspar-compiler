--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright notice, 
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Literal where



import Control.Monad.RWS
import Data.Complex
import GHC.Float (float2Double)

import Language.Syntactic

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Literal

import Feldspar.Range (upperBound)

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation

import Expr        as PIRE
import Program     as PIRE
import Combinators as PIRE
import qualified Types as PIRE
import Procedure as PIRE

import qualified Data.Map as M

instance Compile (Literal :|| Core.Type) dom
  where
    compileExprSym (C' (Literal a)) info Nil m = literal (infoType info) (infoSize info) a  
    
    compileProgSym x@(C' (Literal a)) info k Nil m =
      k $ \name -> foldl1 (.>>) $ map (\(l,i) -> locArray name i l) literals
        where 
          literals = zip (literal (infoType info) (infoSize info) a) 
                         (map Num [0..])
    
    compileProgBasic name (C' (Literal a)) info Nil m = loc name $ head $ literal (infoType info) (infoSize info) a
    

--tellProg $ literalProg (infoType info) (infoSize info) a --Statement $ literal (infoType info) (infoSize info) a
    -- literalLoc (infoType info) (infoSize info) a
--

--literalProg :: TypeRep a -> Core.Size a -> a -> Name -> Program ()
--literalProg t@IntType{}   sz a name = loc name (literal t sz a name) 
--literalProg t@ArrayType{} sz a out  = Alloc PIRE.TInt [] $ \name -> 
--          for (Num 0) (Num 5) $ \e -> loc name (Index name [e]) --locArray name e (literal t sz a name)

--loc name (literal t sz a) 




literal :: TypeRep a -> Core.Size a -> a -> [Expr]
literal t@IntType{}   sz a = literalConst t sz a 
literal t@ArrayType{} sz a = literalConst t sz a --Num $ literalConst t sz a
literal _ _ _ = error "literal undefined."
--literal :: TypeRep a -> Size a -> a -> CodeWriter (Expression ())
--literal t@UnitType        sz a = return (ConstExpr $ literalConst t sz a)
--literal t@BoolType        sz a = return (ConstExpr $ literalConst t sz a)
--literal t@IntType{}       sz a = return (ConstExpr $ literalConst t sz a)
--literal t@FloatType       sz a = return (ConstExpr $ literalConst t sz a)
--literal t@ComplexType{}   sz a = return (ConstExpr $ literalConst t sz a)
--literal t@ArrayType{}     sz a = return (ConstExpr $ literalConst t sz a)
--literal t s a = do loc <- freshVar "x" t s
--                   literalLoc loc t s a
--                   return loc
--

literalConst :: TypeRep a -> Core.Size a -> a -> [Expr]
literalConst t@IntType{}     sz a = [Num $ fromInteger $ toInteger a]
literalConst x@(ArrayType t) sz a = map (head . (literalConst t (defaultSize t))) a -- error "literalConst undefined for Array"


--arrayConst :: TypeRep a -> Core.Size a -> a -> [Expr]
--arrayConst (ArrayType t) sz as = map (Num . literalConst t (defaultSize t)) as



--literalConst :: TypeRep a -> Size a -> a -> Constant ()
--literalConst UnitType        _  ()     = IntConst 0 (Rep.NumType Rep.Unsigned Rep.S32)
--literalConst BoolType        _  a      = BoolConst a
--literalConst trep@IntType{}  sz a      = IntConst (toInteger a) (compileTypeRep trep sz)
--literalConst FloatType       _  a      = FloatConst $ float2Double a
--literalConst (ArrayType t)   sz a      = ArrayConst $ map (literalConst t (defaultSize t)) a
--literalConst (ComplexType t) _  (r:+i) = ComplexConst re ie
--  where re = literalConst t (defaultSize t) r
--        ie = literalConst t (defaultSize t) i
--
--literalLoc :: Location -> TypeRep a -> Size a -> a -> CodeWriter ()
--literalLoc loc arr@ArrayType{} sz e
--    = tellProg [copyProg loc [ConstExpr $ literalConst arr sz e]]
--
--literalLoc loc (Tup2Type ta tb) (sa,sb) (a,b) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--
--literalLoc loc (Tup3Type ta tb tc) (sa,sb,sc) (a,b,c) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--       literalLoc (StructField loc "member3") tc sc c
--       
--literalLoc loc (Tup4Type ta tb tc td) (sa,sb,sc,sd) (a,b,c,d) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--       literalLoc (StructField loc "member3") tc sc c
--       literalLoc (StructField loc "member4") td sd d
--       
--literalLoc loc (Tup5Type ta tb tc td te) (sa,sb,sc,sd,se) (a,b,c,d,e) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--       literalLoc (StructField loc "member3") tc sc c
--       literalLoc (StructField loc "member4") td sd d
--       literalLoc (StructField loc "member5") te se e
--       
--literalLoc loc (Tup6Type ta tb tc td te tf) (sa,sb,sc,sd,se,sf) (a,b,c,d,e,f) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--       literalLoc (StructField loc "member3") tc sc c
--       literalLoc (StructField loc "member4") td sd d
--       literalLoc (StructField loc "member5") te se e
--       literalLoc (StructField loc "member6") tf sf f
--       
--literalLoc loc (Tup7Type ta tb tc td te tf tg) (sa,sb,sc,sd,se,sf,sg) (a,b,c,d,e,f,g) =
--    do literalLoc (StructField loc "member1") ta sa a
--       literalLoc (StructField loc "member2") tb sb b
--       literalLoc (StructField loc "member3") tc sc c
--       literalLoc (StructField loc "member4") td sd d
--       literalLoc (StructField loc "member5") te se e
--       literalLoc (StructField loc "member6") tf sf f
--       literalLoc (StructField loc "member7") tg sg g
--
--literalLoc loc t sz a =
--    do rhs <- literal t sz a
--       assign loc rhs
--
