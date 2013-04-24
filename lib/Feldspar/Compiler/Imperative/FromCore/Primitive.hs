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

module Feldspar.Compiler.Imperative.FromCore.Primitive where



import Language.Syntactic
import Prelude hiding (LT)

import Feldspar.Core.Types (Type)
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Bits
import Feldspar.Core.Constructs.Complex
import Feldspar.Core.Constructs.Conversion
import Feldspar.Core.Constructs.Eq
import Feldspar.Core.Constructs.Floating
import Feldspar.Core.Constructs.Fractional
import Feldspar.Core.Constructs.Integral
import Feldspar.Core.Constructs.Logic
import Feldspar.Core.Constructs.Num
import Feldspar.Core.Constructs.Ord
import Feldspar.Core.Constructs.Trace

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation

import Expr
import Program 

-- | Converts symbols to primitive function calls
instance Compile dom dom => Compile Semantics dom
  where
    compileExprSym (Sem name _) info args m =
        let argExprs = listArgs (head . flip compileExpr m) args
        in [toInfix name argExprs]


    compileProgBasic n nc af (Sem name _) info args m = 
        let argExprs = listArgs (head . flip compileExpr m) args
        in loc n $ head [toInfix name argExprs ]

-- TODO doesn't cover all cases
toInfix :: String -> [Expr] -> Expr
toInfix s es | s == "(*)" , [a,b] <- es = a .* b
             | s == "(+)" , [a,b] <- es = a .+ b
             | s == "(-)" , [a,b] <- es = a .- b
             | s == "(<)" , [a,b] <- es = BinOp $ Expr.LT a b
             | s == "(<=)" , [a,b] <- es = BinOp $ Expr.LTE a b
             | s == "(>)" , [a,b] <- es = BinOp $ Expr.GT a b
             | s == "(>=)" , [a,b] <- es = BinOp $ Expr.GTE a b
             | s == "(==)" , [a,b] <- es = BinOp $ Expr.EQ a b
             | s == "(!=)" , [a,b] <- es = BinOp $ Expr.NEQ a b
             | otherwise  = Call (var s) es

-- | Convenient implementation of 'compileExprSym' for primitive functions
compilePrim :: (Semantic expr, Compile dom dom)
    => (expr :|| Type) a
    -> Info (DenResult a)
    -> Args (AST (Decor Info dom)) a
    -> Alias -> [Expr]
compilePrim (C' s) = compileExprSym $ semantics s

instance Compile dom dom => Compile (BITS       :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (COMPLEX    :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (Conversion :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (EQ         :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (FLOATING   :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (FRACTIONAL :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (INTEGRAL   :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (Logic      :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (NUM        :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (ORD        :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args
instance Compile dom dom => Compile (Trace      :|| Type) dom where compileExprSym = compilePrim
                                                                    compileProgBasic n nc af e info args  = 
                                                                      loc n . head . compilePrim e info args

