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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Binding where

import Data.Typeable

import Control.Monad.RWS
import qualified Data.Map as M
import Data.Maybe

import Language.Syntactic
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding
import qualified Feldspar.Core.Constructs.Binding as Core

import Feldspar.Compiler.Imperative.FromCore.Interpretation

--import Feldspar.Compiler.Imperative.Representation (Expression(..))

import Expr
import Program
import qualified Types as PIRE

instance Compile (Core.Variable :|| Type) dom
  where
    compileExprSym (C' (Core.Variable v)) info Nil m = [var $ variable]
      where variable = fromMaybe (error "Binding  ExprSym: Could not find mapping in Alias for " ++ show v) $ M.lookup v m
    compileProgSym (C' (Core.Variable v)) info k Nil m = k $ \name -> loc name $ var $ variable
      where variable = fromMaybe (error "Binding ProgSym: Could not find mapping in Alias for" ++ show v) $ M.lookup v m

    compileProgBasic name cname (C' (Core.Variable v)) info Nil m = loc name $ var v'
      where v' = fromMaybe (error "Binding ProgBasic: Could not find mapping in Alias for " ++ show v) $ M.lookup v m


instance Compile (CLambda Type) dom
  where
    compileProgSym = error "Can only compile top-level Lambda"
    compileProgBasic = error "Can only compile top-level Lambda: ProgBasic"

instance (Compile dom dom, Project (CLambda Type) dom) => Compile Let dom
  where
  compileProgBasic name cname Let _ (a :* (lam :$ body) :* Nil) m
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam = 
            compileLetWithName a (getInfo lam) v name (M.insert v name' m)
              where (Index name' _) = var "Binding: ThisNameShouldNotMatter"

--  compileProgSym Let _ k (a :* (lam :$ body) :* Nil) m
--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
--        = compileLet a (getInfo lam) v (Alloc PIRE.TInt []) m

--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
--        = do var <- compileLet a (getInfo lam) v
--             withAlias v var $ compileProg loc body
--
--    compileExprSym Let _ (a :* (lam :$ body) :* Nil)
--        | Just (SubConstr2 (Lambda v)) <- prjLambda lam
--        = do var <- compileLet a (getInfo lam) v
--             withAlias v var $ compileExpr body

--compileLet :: Compile dom dom
--           => ASTF (Decor Info dom) a -> Info (a -> b) -> VarId -> ((Name -> Program ()) -> Program ()) -> CodeWriter () --CodeWriter (Expression ())
--compileLet a info v k m = k $ \name -> compileProgWithName (loc name) a (M.insert v name m)

compileLetWithName :: Compile dom dom
           => ASTF (Decor Info dom) a -> Info (a -> b) -> VarId -> Name -> CodeWriter () --CodeWriter (Expression ())
compileLetWithName a info v loc' m = compileProgWithName loc' Nothing a m




--compileLet a info v
--    = do
--        let ta  = argType $ infoType info
--            sa  = infoSize $ getInfo a
--            var = mkVar (compileTypeRep ta sa) v
--        declare var
--        compileProg var a
--        return var
--

compileBind :: Compile dom dom
  => (VarId, ASTB (Decor Info dom) Type) -> CodeWriter ()
compileBind = error "compileBind"
--     = do
--         let info = getInfo e
--             var = mkVar (compileTypeRep (infoType info) (infoSize info)) v
--         declare var
--         compileProg var e


-- TODO size on alloc.
compileBinds :: Compile dom dom
  => ((Name -> Program ()) -> Program ()) 
  -> [(VarId, ASTB (Decor Info dom) Type)]
  -> ASTF (Decor Info dom) a
  -> CodeWriter ()
compileBinds k [] ast m = k $ \out -> let info = getInfo ast
                                          typ  = compileTypeRep (infoType info) (infoSize info)
                                      -- Non-pointer output type does not require Alloc
                                      in case typ of
                                          PIRE.TPointer _ -> Alloc typ [] $ \n c -> compileProgWithName n (Just c) ast m .>> locDeref out (var n)
                                          _               -> Decl typ $ \n -> compileProgWithName n Nothing ast m .>> locDeref out (var n)
compileBinds k ((v, ASTB b):bs) ast m = let info = getInfo b
                                            typ  = compileTypeRep (infoType info) (infoSize info)
                                        in case typ of
                                            PIRE.TPointer _ -> Alloc typ [] $ \n c -> compileProgWithName n (Just c) b m .>> 
                                                                compileBinds k bs ast (M.insert v n m)
                                            _               -> Decl typ $ \n -> compileProgWithName n Nothing b m .>> compileBinds k bs ast (M.insert v n m)

