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

module Feldspar.Compiler.Imperative.FromCore.Tuple where



import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Constructs.Tuple

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance Compile dom dom => Compile (Tuple :|| Type) dom
  where
    compileProgBasic = error "Tuple basic"
--    compileProgSym (C' Tup2) _ loc (m1 :* m2 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--    compileProgSym (C' Tup3) _ loc (m1 :* m2 :* m3 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--        compileProg (StructField loc "member3") m3
--    compileProgSym (C' Tup4) _ loc (m1 :* m2 :* m3 :* m4 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--        compileProg (StructField loc "member3") m3
--        compileProg (StructField loc "member4") m4
--    compileProgSym (C' Tup5) _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--        compileProg (StructField loc "member3") m3
--        compileProg (StructField loc "member4") m4
--        compileProg (StructField loc "member5") m5
--    compileProgSym (C' Tup6) _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--        compileProg (StructField loc "member3") m3
--        compileProg (StructField loc "member4") m4
--        compileProg (StructField loc "member5") m5
--        compileProg (StructField loc "member6") m6
--    compileProgSym (C' Tup7) _ loc (m1 :* m2 :* m3 :* m4 :* m5 :* m6 :* m7 :* Nil) = do
--        compileProg (StructField loc "member1") m1
--        compileProg (StructField loc "member2") m2
--        compileProg (StructField loc "member3") m3
--        compileProg (StructField loc "member4") m4
--        compileProg (StructField loc "member5") m5
--        compileProg (StructField loc "member6") m6
--        compileProg (StructField loc "member7") m7
--
instance Compile dom dom => Compile (Select :|| Type) dom
  where
    compileProgBasic = error "Tuple basic2"
--  where
--    compileExprSym (C' Sel1) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member1"
--    compileExprSym (C' Sel2) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member2"
--    compileExprSym (C' Sel3) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member3"
--    compileExprSym (C' Sel4) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member4"
--    compileExprSym (C' Sel5) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member5"
--    compileExprSym (C' Sel6) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member6"
--    compileExprSym (C' Sel7) _ (tup :* Nil) = do
--        tupExpr <- compileExpr tup
--        return $ StructField tupExpr "member7"
--
