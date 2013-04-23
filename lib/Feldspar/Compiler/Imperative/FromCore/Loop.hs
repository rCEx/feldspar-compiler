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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Loop where

import Data.Typeable (Typeable(..))

import Prelude hiding (init)

import qualified Data.Map as M

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Loop hiding (For, While)
import Feldspar.Core.Constructs.Literal
import qualified Feldspar.Core.Constructs.Loop as Core

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Binding (compileBind)

import Expr
import Program

instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         , Project Let dom
         , ConstrainedBy dom Typeable
         )
      => Compile (Loop :|| Type) dom
  where
    -- TODO this doesn't compile let binds
    compileProgSym (C' ForLoop) _ k (len :* init :* (lam1 :$ lt1) :* Nil) m
        | Just (SubConstr2 (Lambda ix)) <- prjLambda lam1
        , (bs1, (lam2 :$ ixf)) <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda st)) <- prjLambda lam2
        = let  ta  = argType $ infoType $ getInfo lam1
               sa  = fst $ infoSize $ getInfo lam1
               typ = compileTypeRep ta sa
               initExpr = head $ compileExpr init m
          in k $ \out -> Decl typ $ \im -> loc im (deref $ var out) 
         .>> (for initExpr (head $ compileExpr len m) $ \e ->
               loc im $ head $ compileExpr ixf $ M.insert st im $ M.insert ix (nameFromVar e) m) 
         .>> locDeref out $ var im
              

        --do
--            blocks <- mapM (confiscateBlock . compileBind) bs1
--            let info1 = getInfo lam1
--                info2 = getInfo lam2
--                sz = fst $ infoSize info1
--                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
--            let ix' = mkVar (compileTypeRep (infoType info1) (infoSize info1)) ix
--            let stvar = mkVar (compileTypeRep (infoType info2) (infoSize info2)) st
--            len' <- mkLength len (infoType $ getInfo len) sz
--            compileProg loc init
--            (_, Block ds body) <- withAlias st loc $ confiscateBlock $ compileProg stvar ixf >> assign loc stvar
--            declare stvar
--            tellProg [toProg $ Block (concat dss ++ ds) (for (lName ix') len' 1 (toBlock $ Sequence $ concat lets ++ [body]))]

    compileProgBasic out (C' ForLoop) _ (len :* init :* (lam1 :$ lt1) :* Nil) m
        | Just (SubConstr2 (Lambda ix)) <- prjLambda lam1
        , (bs1, (lam2 :$ ixf)) <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda st)) <- prjLambda lam2
        = let  ta    = argType $ infoType $ getInfo lam1
               sa    = fst $ infoSize $ getInfo lam1
               typ   = compileTypeRep ta sa
               start = head $ compileExpr init m
               end   = head $ compileExpr len m
          in for start end $ \e ->
               loc out $ head $ compileExpr ixf $ M.insert st out $ M.insert ix (nameFromVar e) m

--Decl typ $ \intermed -> loc intermed (var out) 
--         .>> (for start end $ \e ->
--               loc intermed $ head $ compileExpr ixf $ M.insert st intermed $ M.insert ix (nameFromVar e) m) 
--         .>> loc out $ var intermed

    compileProgBasic _ _ _ _ _= error "Loop  basic"

--
--    compileProgSym (C' WhileLoop) _ loc (init :* (lam1 :$ cond) :* (lam2 :$ body) :* Nil)
--        | Just (SubConstr2 (Lambda cv)) <- prjLambda lam1
--        , Just (SubConstr2 (Lambda cb)) <- prjLambda lam2
--        = do
--            let info2 = getInfo lam2
--                info1 = getInfo lam1
--            let stvar = mkVar (compileTypeRep (infoType info2) (infoSize info2)) cb
--                condv = mkVar (compileTypeRep (infoType info1) (infoSize info1)) cv
--            compileProg loc init
--            (_, cond') <- confiscateBlock $ withAlias cv loc $ compileProg condv cond
--            (_, body') <- withAlias cb loc $ confiscateBlock $ compileProg stvar body >> assign loc stvar
--            declare stvar
--            declare condv
--            tellProg [while cond' condv body']
--
instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         )
      => Compile (LoopM Mut) dom
  where
    compileProgBasic = error "LoopM basic"
--    compileProgSym Core.For _ loc (len :* (lam :$ ixf) :* Nil)
--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
--        = do
--            let ta = argType $ infoType $ getInfo lam
--            let sa = fst $ infoSize $ getInfo lam
--            let ix = mkVar (compileTypeRep ta sa) v
--            len' <- mkLength len (infoType $ getInfo len) sa
--            (_, Block ds body) <- confiscateBlock $ compileProg loc ixf
--            tellProg [toProg $ Block ds (for (lName ix) len' 1 (toBlock body))]
--
---- TODO Missing While
--    compileProgSym Core.While _ loc (cond :* step :* Nil)
--        = do
--            let info1 = getInfo cond
--            condv <- freshVar "cond" (infoType info1) (infoSize info1)
--            (_, cond')          <- confiscateBlock $ compileProg condv cond
--            (_, step') <- confiscateBlock $ compileProg loc step
--            tellProg [while cond' condv step']
--
