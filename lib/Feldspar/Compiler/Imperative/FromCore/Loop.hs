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
import Data.Maybe

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
import Feldspar.Compiler.Imperative.FromCore.Binding

import Expr
import Program
import qualified Types as PIRE

instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         , Project Let dom
         , ConstrainedBy dom Typeable
         )
      => Compile (Loop :|| Type) dom
  where
    compileExprSym (C' ForLoop) _ (len :* init :* (lam1 :$ lt1) :* Nil) m = error "compileExprSym forLoop"

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
              compileProgWithName (var im, loc im) Nothing Nothing ixf (M.insert st (var im) $ M.insert ix e m))
         .>> locDeref out $ var im

    compileProgBasic out outc af (C' ForLoop) _ (len :* init :* (lam1 :$ lt1) :* Nil) m
        | Just (SubConstr2 (Lambda ix)) <- prjLambda lam1
        , (bs1, (lam2 :$ ixf)) <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda st)) <- prjLambda lam2
        = let  ta    = argType $ infoType $ getInfo lam1
               sa    = fst $ infoSize $ getInfo lam1
               typ   = compileTypeRep ta sa
               ta2   = argType $ infoType $ getInfo lam2
               sa2   = fst $ infoSize $ getInfo lam2
               typ2  = compileTypeRep ta2 sa2
               bound = head $ compileExpr len m
               ta3   = infoType $ getInfo ixf
               sa3   = infoSize $ getInfo ixf
               typ3 = compileTypeRep ta3 sa3
          in --maybe Skip (\f -> f [bound]) af .>>
            case typ3 of PIRE.TPointer _ ->  compileProgWithName out outc af init m .>>
                                             -- for (Num 0) bound $ \e -> 
                                             -- compileLets bs1
                                             --             (compileProgWithName out outc Nothing ixf)
                                             --             (M.insert st (fst out) $ M.insert ix e m)

                                              Alloc typ3 $ \temp tempc tempAf -> --let (Assign _ xs _) = snd out undefined
                                                                                 --    (Index n _)     = fst out
                                                                                 --in
                                              --loc temp (Index n xs) .>>
                                              for (Num 0) bound $ \e -> 
                                               compileLets bs1
                                                           (compileProgWithName (var temp, memcpy (var temp) (var tempc) PIRE.TInt) (Just tempc) (Just tempAf) ixf)
                                                           (M.insert st (fst out) $ M.insert ix e m)
                                               .>> memcpy (fst out) (var tempc) PIRE.TInt (var temp) 
                                               .>> free (var temp)
                                               -- snd out (var temp)


                         _               -> compileProgWithName out outc af init m .>>
                                             Decl typ3 $ \temp -> let (Assign _ xs _) = snd out (undefined)
                                                                      (Index n _)     = fst out
                                                                  in 
                                               loc temp (Index n xs) .>>
                                               for (Num 0) bound (\e -> 
                                               compileLets bs1 
                                                           (compileProgWithName (var temp, loc temp) Nothing Nothing ixf)
                                                           (M.insert st (var temp) $ M.insert ix e m))
                                               .>> snd out (var temp)

    compileProgBasic _ _ _ _ _ _ _ = error "Loop basic"

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

