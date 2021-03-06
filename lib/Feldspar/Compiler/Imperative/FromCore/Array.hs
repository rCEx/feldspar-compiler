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

module Feldspar.Compiler.Imperative.FromCore.Array where


import Data.List (init)
import Data.Typeable
import Data.Maybe

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Constructs.Tuple
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Constructs.Literal

import Feldspar.Compiler.Imperative.Frontend
--import qualified Feldspar.Compiler.Imperative.Representation as Rep (Type(..))
--import Feldspar.Compiler.Imperative.Representation (Expression(..), Program(..),
--                                                    Block(..), Size(..),
--                                                    Signedness(..), typeof)
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Binding

import Program
import Expr
import Procedure
import qualified Types as PIRE

import qualified Data.Map as M

import Debug.Trace

instance ( Compile dom dom
         , Project (CLambda Type) dom
         , Project (Literal  :|| Type) dom
         , Project (Variable :|| Type) dom
         , Project Let dom
         , Project (Array :|| Type) dom
         , Project (Tuple :|| Type) dom
         , ConstrainedBy dom Typeable
         , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
         )
      => Compile (Array :|| Type) dom
  where
    compileProgSym (C' Parallel) info k (len :* (lam :$ ixf) :* Nil) m
      | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        =  let ta  = argType $ infoType $ getInfo lam
               sa  = fst $ infoSize $ getInfo lam
               typ = compileTypeRep ta sa
           in k $ \name -> par (Num 0) (head $ compileExpr len m) $ \e ->
                    compileProgWithName (var name, locArray name e) Nothing Nothing ixf (M.insert v e m)
    
    compileProgSym (C' Sequential) _ k (len :* st :* (lam1 :$ lt1) :* Nil) m
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , (bs1, (lam2 :$ step)) <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        = let ta1  = argType $ infoType $ getInfo lam1
              sa1  = fst $ infoSize $ getInfo lam1
              typ1 = compileTypeRep ta1 sa1
              ta2  = argType $ infoType $ getInfo lam2
              sa2  = fst $ infoSize $ getInfo lam2
              typ2 = compileTypeRep ta2 sa2
          in k $ \name -> Decl typ2 $ \stName -> loc stName (head $ compileExpr st m) 
          .>> loc stName (Num 0)
          .>> (for (Num 0) (head $ compileExpr len m) $ \e -> 
                compileProgWithName (var stName, locArray stName e) Nothing Nothing step (M.insert v (var stName) (M.insert s e m)))
          .>> loc name $ var stName
                
                  
--            blocks <- mapM (confiscateBlock . compileBind) bs1
--            let t = argType $ infoType $ getInfo lam1
--            let sz = fst $ infoSize $ getInfo lam1
--            let tr' = resType $ infoType $ getInfo lam2
--            let sr' = snd $ infoSize $ getInfo lam2
--            let ix = mkVar (compileTypeRep t sz) v
--                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
--            len' <- mkLength len (infoType $ getInfo len) sz
--            tmp  <- freshVar "seq" tr' sr'
--            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg tmp step
--            tellProg [initArray (AddrOf loc) len']
--            compileProg (StructField tmp "member2") st
--            tellProg [toProg $ Block (concat dss ++ ds) $
--                      for (lName ix) len' 1 $
--                                    toBlock $ Sequence (concat lets ++ body ++
--                                         [copyProg (ArrayElem (AddrOf loc) ix) [StructField tmp "member1"]
--                                         ])]
   -- compileProgSym (C' Sequential) _ k (len :* st :* (lam1 :$ lt1) :* Nil) m
   --         | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
   --         , (bs1, (lam2 :$ step)) <- collectLetBinders lt1
   --         , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
   --         = error "Sequential2"

  
    compileExprSym (C' GetLength) _ (a :* Nil) m | [Index mem n is] <- compileExpr a m = [Index mem (n ++ "c") is] -- TODO: assumes a parameter is used.
    compileExprSym (C' GetIx) _ (arr :* i :* Nil) m | [Index mem n is] <- compileExpr arr m = [Index mem n (compileExpr i m)]
    compileExprSym (C' SetIx) info args m = error "Array ExprSym1"
    compileExprSym (C' SetLength) info args m = error "Array ExprSym2"
    compileExprSym (C' Append) info args m = error "Array ExprSym3"
    compileExprSym (C' Sequential) info args m = error "Array ExprSym5"

    compileExprSym (C' Parallel) info args@(len :* (lam :$ ixf) :* Nil) m      
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam 
        = error "compileExprSym not implemented for Parallel. Try calling with compileProgWithName."
       -- let ta  = argType $ infoType $ getInfo lam
       --        sa  = fst $ infoSize $ getInfo lam
       --        typ = compileTypeRep ta sa
       --    in error "ExprSym Parallel"
           -- TODO can't return the name?! Call this via compileProgWithName instead of compileExpr



    compileProgBasic name namec af (C' Parallel) info (len :* (lam :$ ixf) :* Nil) m
      | Just (SubConstr2 (Lambda v)) <- prjLambda lam
        =  let ta = argType $ infoType $ getInfo lam
               sa = fst $ infoSize $ getInfo lam
               typ = compileTypeRep ta sa
               bound = (head $ compileExpr len m)
               (Index _ name' _) = fst name
           in maybe Skip (\f -> f DevGlobal [bound]) af -- XXXX
          .>> par (Num 0) bound $ \e -> 
               compileProgWithName (fst name, locArray (name') e) Nothing Nothing ixf 
                (M.insert v e m)


    compileProgBasic name namec af (C' Sequential) _ (len :* st :* (lam1 :$ lt1) :* Nil) m
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
        , (bs1, (lam2 :$ step)) <- collectLetBinders lt1
        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
        = let ta1  = argType $ infoType $ getInfo lam1
              sa1  = fst $ infoSize $ getInfo lam1
              typ1 = compileTypeRep ta1 sa1
              ta2  = argType $ infoType $ getInfo lam2
              sa2  = fst $ infoSize $ getInfo lam2
              typ2 = compileTypeRep ta2 sa2
              bound = head $ compileExpr len m
          in maybe Skip (\f -> f DevGlobal [bound]) af -- XXXX
         .>> Decl typ2 $ \stName -> compileProgWithName (var stName, loc stName) Nothing Nothing st m
         .>> loc stName (Num 0)
         .>> for (Num 0) bound $ \e -> compileProgWithName (var stName, locArray stName e) Nothing Nothing step 
                                        (M.insert v e (M.insert s (var stName) m))
         .>> snd name $ var stName

    compileProgBasic _ _ _ (C' SetLength) _ (len :* arr :* Nil) m = error "setLength basic"
    compileProgBasic name namec af (C' GetLength) _ (a :* Nil) m = let [Index mem n is] = compileExpr a m in snd name $  Index mem (n ++ "c") is
          --snd name $ head $ compileExpr a m
    compileProgBasic name namec af (C' GetIx) _ (arr :* i :* Nil) m
      | [Index mem n is] <- compileExpr arr m
      = maybe Skip (\f -> f mem [Index Host (n ++ "c") []]) af .>> -- TODO causes memory leak -- XXXX
        snd name $ Index mem n (compileExpr i m)
    compileProgBasic _ _ _ (C' SetIx) _ (arr :* i :* a :* Nil) m  = error "SetIx basic"
    compileProgBasic _ _ _ (C' Append) _ ((arr1 :$ l1 :$ (lam1 :$ body1)) :* (arr2 :$ l2 :$ (lam2 :$ body2)) :* Nil) m = error "Append basic"
    compileProgBasic _ _ _ (C' Append) _ (a :* b :* Nil) m = error "Append basic2"
    compileProgBasic _ _ _ _ _ _ _ = error $ "compileProgBasic undefined for Array"


               --len' = mkLength len (infoType $ getInfo len) sa m
              -- in --Alloc typ [] $ \lenName -> 
              --      k $ \name -> for (Num 0) (Num 10) $ \e -> (compileProg k ixf m) --locArray name e (
                --for (Num 0) len' $ \e -> loc name e--compileProg ixf m
               --mkLength len (infoType $ getInfo len) sa k m
           --in k $ \name -> procbody $ for (num 0) (num 25) $ \e -> loc name e--compileprog ixf m
--        = do  let ta = argType $ infoType $ getInfo lam
--              tellProg $ 

--            let ta = argType $ infoType $ getInfo lam
--            let sa = fst $ infoSize $ getInfo lam
--            let ix = mkVar (compileTypeRep ta sa) v
--            len' <- mkLength len (infoType $ getInfo len) sa
--            (_, b) <- confiscateBlock $ compileProg (ArrayElem (AddrOf loc) ix) ixf
--            tellProg [initArray (AddrOf loc) len']
--            tellProg [for (lName ix) len' 1 b]
--
--
--    compileProgSym (C' Sequential) _ loc (len :* init' :* (lam1 :$ lt1) :* Nil)
--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
--        , (bs1, (lam2 :$ l)) <- collectLetBinders lt1
--        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
--        , (bs, (tup :$ a :$ b))        <- collectLetBinders l
--        , Just (C' Tup2)               <- prjF tup
--        , (e, ASTB step)               <- last bs
--        , Just (C' (Variable t1))      <- prjF a
--        , Just (C' (Variable t2))      <- prjF b
--        , t1 == e
--        , t2 == e
--        = do
--            blocks <- mapM (confiscateBlock . compileBind) (bs1 ++ init bs)
--            let tix = argType $ infoType $ getInfo lam1
--                six = fst $ infoSize $ getInfo lam1
--                tst = infoType $ getInfo step
--                sst = infoSize $ getInfo step
--                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
--            let ix = mkVar (compileTypeRep tix six) v
--            len' <- mkLength len (infoType $ getInfo len) six
--            st1 <- freshVar "st" tst sst
--            let st = mkRef (compileTypeRep tst sst) s
--            declareAlias st
--            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st $ compileProg (ArrayElem (AddrOf loc) ix) step
--            withAlias s st $ compileProg st1 init'
--            tellProg [ Assign (AddrOf st) (AddrOf st1)
--                     , initArray (AddrOf loc) len']
--            tellProg [toProg $ Block (concat dss ++ ds) $
--                      for (lName ix) len' 1 $
--                                    toBlock $ Sequence (concat lets ++ body ++
--                                         [Assign (AddrOf st) (AddrOf $ ArrayElem (AddrOf loc) ix)
--                                         ])]
--
--    compileProgSym (C' Sequential) _ loc (len :* st :* (lam1 :$ lt1) :* Nil)
--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam1
--        , (bs1, (lam2 :$ step)) <- collectLetBinders lt1
--        , Just (SubConstr2 (Lambda s)) <- prjLambda lam2
--        = do
--            blocks <- mapM (confiscateBlock . compileBind) bs1
--            let t = argType $ infoType $ getInfo lam1
--            let sz = fst $ infoSize $ getInfo lam1
--            let tr' = resType $ infoType $ getInfo lam2
--            let sr' = snd $ infoSize $ getInfo lam2
--            let ix = mkVar (compileTypeRep t sz) v
--                (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
--            len' <- mkLength len (infoType $ getInfo len) sz
--            tmp  <- freshVar "seq" tr' sr'
--            (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg tmp step
--            tellProg [initArray (AddrOf loc) len']
--            compileProg (StructField tmp "member2") st
--            tellProg [toProg $ Block (concat dss ++ ds) $
--                      for (lName ix) len' 1 $
--                                    toBlock $ Sequence (concat lets ++ body ++
--                                         [copyProg (ArrayElem (AddrOf loc) ix) [StructField tmp "member1"]
--                                         ])]
--
--    -- loc = parallel l f ++ parallel l g ==> for l (\i -> loc[i] = f i; loc[i+l] = g i)
--    compileProgSym (C' Append) _ loc ((arr1 :$ l1 :$ (lam1 :$ body1)) :* (arr2 :$ l2 :$ (lam2 :$ body2)) :* Nil)
--        | Just (C' Parallel) <- prjF arr1
--        , Just (C' Parallel) <- prjF arr2
--        , Just (SubConstr2 (Lambda v1)) <- prjLambda lam1
--        , Just (SubConstr2 (Lambda v2)) <- prjLambda lam2
--        , alphaEq l1 l2
--        = do
--            let t   = argType $ infoType $ getInfo lam1
--                sz  = fst $ infoSize $ getInfo lam1
--                ix1 = mkVar (compileTypeRep t sz) v1
--                ix2 = mkVar (compileTypeRep t sz) v2
--            len <- mkLength l1 (infoType $ getInfo l1) sz
--            (_, Block ds1 (Sequence b1)) <- confiscateBlock $ withAlias v1 ix1 $ compileProg (ArrayElem (AddrOf loc) ix1) body1
--            (_, Block ds2 (Sequence b2)) <- confiscateBlock $ withAlias v2 ix1 $ compileProg (ArrayElem (AddrOf loc) ix2) body2
--            tellProg [initArray (AddrOf loc) len]
--            assign ix2 len
--            tellProg [for (lName ix1) len 1 (Block (ds1++ds2) (Sequence $ b1 ++ b2 ++ [copyProg ix2 [(binop (Rep.NumType Unsigned S32) "+" ix2 (litI32 1))]]))]
--
--    compileProgSym (C' Append) _ loc (a :* b :* Nil) = do
--        a' <- compileExpr a
--        b' <- compileExpr b
--        tellProg [copyProg (AddrOf loc) [a', b']]
--        -- TODO: Optimize by writing to directly to 'loc' instead of 'a'' and 'b''!
--        --       But take care of array initialization:
--        --       compiling 'a' and 'b' might do initialization itself...
--
--    compileProgSym (C' SetIx) _ loc (arr :* i :* a :* Nil) = do
--        compileProg loc arr
--        i' <- compileExpr i
--        compileProg (ArrayElem (AddrOf loc) i') a
--    compileExprSym (C' GetLength) info (a :* Nil) = error "getLength"
       -- do
       -- aExpr <- compileExpr a
       -- return $ arrayLength aExpr
--    compileProgSym (C' GetIx) _ loc (arr :* i :* Nil) = do
--        a' <- compileExpr arr
--        i' <- compileExpr i
--        let el = ArrayElem (AddrOf a') i'
--        if isArray $ typeof el
--          then tellProg [Sequence [Assign (AddrOf loc) (AddrOf el)]]
--          else tellProg [copyProg loc [el]]
--
--    compileProgSym (C' SetLength) _ loc (len :* arr :* Nil) = do
--        len' <- compileExpr len
--        compileProg loc arr
--        tellProg [setLength (AddrOf loc) len']
--      -- TODO Optimize by using copyProgLen (compare to 0.4)
--
--    compileProgSym a info loc args = compileExprLoc a info loc args
--
--
--    compileExprSym (C' GetIx) _ (arr :* i :* Nil) = do
--        a' <- compileExpr arr
--        i' <- compileExpr i
--        return $ ArrayElem (AddrOf a') i'
--
--    compileExprSym a info args = compileProgFresh a info args
--
