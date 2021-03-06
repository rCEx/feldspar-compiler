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
    
    compileProgSym (C' (Literal a)) info k Nil m =
      k $ \name -> foldl1 (.>>) $ locs name
        where 
          locs name = zipWith ($) (map (locArray name . Num) [0..]) literals
          literals  = literal (infoType info) (infoSize info) a
    
    compileProgBasic name namec af (C' (Literal a)) info Nil m = 
      let x = literal (infoType info) (infoSize info) a in
      maybe Skip (\f -> f Host x) af .>> snd name $ head $ x -- XXXX
    

literal :: TypeRep a -> Core.Size a -> a -> [Expr]
literal t@IntType{}   sz a = literalConst t sz a 
literal t@ArrayType{} sz a = literalConst t sz a
literal _ _ _ = error "literal undefined."


literalConst :: TypeRep a -> Core.Size a -> a -> [Expr]
literalConst t@IntType{}     sz a = [Num $ fromInteger $ toInteger a]
literalConst (ArrayType t) sz a = map (head . (literalConst t (defaultSize t))) a 


