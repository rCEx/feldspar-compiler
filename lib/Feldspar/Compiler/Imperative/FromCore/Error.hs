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

module Feldspar.Compiler.Imperative.FromCore.Error where



import Control.Monad.RWS

import Language.Syntactic

import Feldspar.Core.Types (Type)
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Error

import Feldspar.Compiler.Imperative.Frontend
--import Feldspar.Compiler.Imperative.Representation (Program(..),
--                                                    ActualParameter(..))
import Feldspar.Compiler.Imperative.FromCore.Interpretation



instance (Compile dom dom) => Compile (Error :|| Type) dom
  where
    compileProgSym (C' Undefined)    _ k  Nil = error "Error compileProgSym"--return ()
    compileProgBasic = error "Error basic"
--    compileProgSym (C' (Assert msg)) _ loc (cond :* a :* Nil) = do
--        compileAssert cond msg
--        compileProg loc a
--
--    compileExprSym (C' (Assert msg)) _ (cond :* a :* Nil) = do
--        compileAssert cond msg
--        compileExpr a
--    compileExprSym a info args = compileProgFresh a info args
--
--compileAssert :: (Compile dom dom)
--              => ASTF (Decor Info dom) a -> String -> CodeWriter ()
--compileAssert cond msg = do
--    condExpr <- compileExpr cond
--    tellProg [call "assert" [In condExpr]]
--    unless (null msg) $ tellProg [Comment False $ "{" ++ msg ++ "}"]

