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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Feldspar.Compiler.Imperative.FromCore where


import Data.List (nub)
import Data.Typeable
import qualified Data.Map as M

import Control.Monad.RWS
import Control.Monad.Writer

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types
import Feldspar.Core.Interpretation as Interp
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Literal
import Feldspar.Core.Constructs.Binding
import Feldspar.Core.Frontend
import Feldspar.Core.Interpretation

import Feldspar.Range (upperBound)

--import qualified Feldspar.Compiler.Imperative.Representation as Rep (Variable(..), Type(..))
--import Feldspar.Compiler.Imperative.Representation (ActualParameter(..), Expression(..), Program(..), Block(..), Module(..), Entity(..), Declaration(..))
--import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.FromCore.Array ()
import Feldspar.Compiler.Imperative.FromCore.Binding
import Feldspar.Compiler.Imperative.FromCore.Condition ()
import Feldspar.Compiler.Imperative.FromCore.ConditionM ()
import Feldspar.Compiler.Imperative.FromCore.Error ()
import Feldspar.Compiler.Imperative.FromCore.FFI ()
import Feldspar.Compiler.Imperative.FromCore.Future ()
import Feldspar.Compiler.Imperative.FromCore.Literal ()
import Feldspar.Compiler.Imperative.FromCore.Loop ()
import Feldspar.Compiler.Imperative.FromCore.Mutable ()
import Feldspar.Compiler.Imperative.FromCore.MutableToPure ()
import Feldspar.Compiler.Imperative.FromCore.NoInline ()
import Feldspar.Compiler.Imperative.FromCore.Par ()
import Feldspar.Compiler.Imperative.FromCore.Primitive ()
import Feldspar.Compiler.Imperative.FromCore.Save ()
import Feldspar.Compiler.Imperative.FromCore.SizeProp ()
import Feldspar.Compiler.Imperative.FromCore.SourceInfo ()
import Feldspar.Compiler.Imperative.FromCore.Tuple ()
--
import Feldspar.Compiler.Backend.C.Options (Options(..))

import Program         as PIRE
import Expr            as PIRE
import qualified Types as PIRE
import Combinators     as PIRE
import Procedure       as PIRE
import GenOCL          as PIRE
import Gen             as PIRE

import Debug.Trace

instance Compile FeldDom FeldDom
  where
    compileProgSym (C' a)          = compileProgSym a--traceShow ("TRACE on ProgSym: " ++ show a) compileProgSym a 
    compileProgBasic n m af (C' a) = compileProgBasic n m af a--traceShow ("TRACE on ProgBasic: " ++ show a) compileProgBasic n a 
    compileExprSym (C' a)          = compileExprSym a --traceShow ("TRACE on ExprSym: " ++ show a) compileExprSym a

instance Compile Empty dom
  where
    compileProgSym _   = error "Can't compile Empty"
    compileProgBasic _ = error "Can't compile Empty"
    compileExprSym _   = error "Can't compile Empty"




compileProgTop :: ( Compile dom dom
                  , Project (CLambda Type) dom
                  , Project Let dom
                  , Project (Literal :|| Type) dom
                  , ConstrainedBy dom Typeable
                  ) =>
          [(VarId, ASTB (Decor Info dom) Type)] ->
          ASTF (Decor Info dom) a -> CodeWriter ()
compileProgTop bs (lam :$ body) m
    | Just (SubConstr2 (Lambda v)) <- prjLambda lam
    = let ta  = argType $ infoType $ getInfo lam
          sa  = fst $ infoSize $ getInfo lam
          typ = compileTypeRep ta sa
      in InParam typ $ \mem name -> case typ of
                                      PIRE.TPointer t -> case mem of  --FIXME case on typ to be Pointer, Not scalar!
                                                            Host      -> compileProgTop bs body (M.insert v (var name) m)
                                                            DevGlobal -> Alloc typ $ \name' namec' af -> af mem [var $ name ++ "c"] .>>
                                                                          memcpy (glob name') (var namec') t (var name) .>>
                                                                          compileProgTop bs body (M.insert v (glob name') m) .>>
                                                                          free (glob name')
                                      _           -> compileProgTop bs body (M.insert v (var name) m)
                                     
compileProgTop bs (lt :$ e :$ (lam :$ body)) m
  | Just (SubConstr2 (Lambda v)) <- prjLambda lam
  , Just Let <- prj lt
  , Just (C' Literal{}) <- prjF e -- Input on form let x = n in e
--  , [ProcedureCall "copy" [Out (VarExpr vr), In (ConstExpr c)]] <- bd
--  , freshName Prelude.== vName vr -- Ensure that compiled result is on form x = n
  = error "compileProgTop: LetBinding with lambda NYI."--do tellDef [ValueDef var c]
--       withAlias v (varToExpr var) $
--         compileProgTop opt funname bs body
--  where
--    info     = getInfo e
--    outType  = case compileTypeRep (infoType info) (infoSize info) of
--                 Rep.Pointer (Rep.ArrayType rs t) -> Rep.NativeArray (Just $ upperBound rs) t
--                 t -> t
--    var@(Rep.Variable _ freshName) = case prjLambda lam of
--               Just (SubConstr2 (Lambda v)) -> mkVariable outType v
--    bd = sequenceProgs $ blockBody $ block $ snd $
--          evalRWS (compileProg (varToExpr var) e) (initReader opt) initState
compileProgTop bs e@(lt :$ _ :$ _) m
  | Just Let <- prj lt
  , (bs', body) <- collectLetBinders e
  = compileProgTop (reverse bs' ++ bs) body m



--compileProgTop opt funname bs a = do
--    let
--        info       = getInfo a
--        outType    = Rep.Pointer $ compileTypeRep (infoType info) (infoSize info)
--        outParam   = Rep.Variable outType "out"
--        outLoc     = varToExpr outParam
--    mapM compileBind (reverse bs)
--    compileProg outLoc a
--    return outParam

compileProgTop bs a m = compileBinds (OutParam $ PIRE.TPointer typ) (reverse bs) a m
  where info = getInfo a
        typ = compileTypeRep (infoType info) (infoSize info)


fromCore :: SyntacticFeld a => a -> PIRE.Program ()
fromCore prog = BasicProc result
  where
    result = compileProgTop [] ast M.empty
    ast        = reifyFeld (frontendOpts opt) N32 prog
    opt        = Options {frontendOpts = defaultFeldOpts}


    --runRWS (compileProgTop [] ast) () initState --evalRWS (compileProgTop [] ast) (initReader opt) initState

--    decls      = decl results
--    ins        = args results
--    post       = epilogue results
--    Block ds p = block results
--    paramTypes = getTypes opt $ Declaration outParam Nothing:map (\v -> Declaration v Nothing) ins
--    defs       =  nub (def results ++ paramTypes)
--               ++ [ProcDef funname ins [outParam] (Block (ds ++ decls) (Sequence (p:post)))]

-- | Get the generated core for a program.
--getCore' :: SyntacticFeld a => Options -> a -> Module ()
--getCore' opts = fromCore opts "test"
