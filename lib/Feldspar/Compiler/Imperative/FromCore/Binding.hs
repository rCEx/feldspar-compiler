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
    compileExprSym (C' (Core.Variable v)) info Nil m = [variable]
      where variable = fromMaybe (error $ "Binding  ExprSym: Could not find mapping in Alias for " ++ show v ++ ", map is: " ++ show m) 
                         $ M.lookup v m
    compileProgSym (C' (Core.Variable v)) info k Nil m = k $ \name -> loc name variable
      where variable = fromMaybe (error $ "Binding ProgSym: Could not find mapping in Alias for " ++ show v) $ M.lookup v m

    compileProgBasic name cname af (C' (Core.Variable v)) info Nil m = maybe Skip (\f -> f DevGlobal [case v' of -- XXXX
                                                                                              Index m v is -> Index m (v ++ "c") is
                                                                                              a            -> a
                                                                                  ]) af .>>
                                                                       snd name v'
      where v'= fromMaybe (error $ "Binding ProgBasic: Could not find mapping in Alias for " ++ show v) $ M.lookup v m


instance Compile (CLambda Type) dom
  where
    compileProgSym = error "Can only compile top-level Lambda: ProgSym"
    compileProgBasic = error "Can only compile top-level Lambda: ProgBasic"

instance (Compile dom dom, Project (CLambda Type) dom) => Compile Let dom
  where
  compileProgBasic name cname af Let _ (a :* (lam :$ body) :* Nil) m
        | Just (SubConstr2 (Lambda v)) <- prjLambda lam = 
            let info = getInfo a
                typ  = compileTypeRep (infoType info) (infoSize info)
            in --maybe Skip (\f -> f [var $ fromJust cname]) af .>> 
              case typ of
                 PIRE.TPointer _ -> Alloc typ $ \n c af' -> 
                                      compileProgWithName (glob n, loc n) (Just c) (Just af') a (M.insert v (glob n) m) .>>
                                      compileProgWithName name cname af body (M.insert v (glob n) m)
                                    .>> free (glob n)
                                    --  compileLetWithName a (getInfo lam) v name (M.insert v n m)
                                    --  .>> compileProgWithName name (Just c) (Just af) body (M.insert v n m)
                 _               -> Decl typ $ \n ->
                                      compileProgWithName (var n, loc n) Nothing Nothing a (M.insert v (var n) m) .>>
                                      compileProgWithName name cname af body (M.insert v (var n) m)

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

--compileLetWithName :: Compile dom dom
--           => ASTF (Decor Info dom) a -> Info (a -> b) -> VarId -> (Name, Loc Expr ()) -> CodeWriter () --CodeWriter (Expression ())
--compileLetWithName a info v name m = compileProgWithName name Nothing Nothing a m




--compileLet a info v
--    = do
--        let ta  = argType $ infoType info
--            sa  = infoSize $ getInfo a
--            var = mkVar (compileTypeRep ta sa) v
--        declare var
--        compileProg var a
--        return var

compileLets :: Compile dom dom
  => [(VarId, ASTB (Decor Info dom) Type)]
  -> (Alias -> Program ())
  -> CodeWriter ()
compileLets [] f m               = f m
compileLets ((v, ASTB b):bs) f m = let info = getInfo b
                                       typ  = compileTypeRep (infoType info) (infoSize info)
                                   in case typ of
                                       PIRE.TPointer _ -> Alloc typ $ \n c af -> 
                                                            compileProgWithName (glob n, loc n) (Just c) (Just af) b (M.insert v (glob n) m)
                                                        .>> compileLets bs f (M.insert v (glob n) m)
                                                        .>> free (glob n)
                                       _               -> Decl typ $ \n -> 
                                                            compileProgWithName (var n, loc n) Nothing Nothing b (M.insert v (var n) m)
                                                        .>> compileLets bs f (M.insert v (var n) m)


compileBinds :: Compile dom dom
  => ((Name -> Program ()) -> Program ()) 
  -> [(VarId, ASTB (Decor Info dom) Type)]
  -> ASTF (Decor Info dom) a
  -> CodeWriter ()
compileBinds k [] ast m = k $ \out -> let info = getInfo ast
                                          typ  = compileTypeRep (infoType info) (infoSize info)
                                      in case typ of
                                          PIRE.TPointer t -> Alloc typ $ \n c af ->
                                                               compileProgWithName (glob n, memcpy (glob n) (var c) t) (Just c) (Just af) ast m 
                                                              .>> memcpy (deref $ var out) (var c) t (glob n)
                                                              .>> free (glob n)
                                                                --locDeref out (var n)
                                          _               -> Decl typ $ \n -> 
                                                              compileProgWithName (var n, loc n) Nothing Nothing ast m 
                                                          .>> locDeref out (var n)
compileBinds k ((v, ASTB b):bs) ast m = let info = getInfo b
                                            typ  = compileTypeRep (infoType info) (infoSize info)
                                        in case typ of
                                            PIRE.TPointer t -> Alloc typ $ \n c af -> 
                                                                compileProgWithName (glob n, memcpy (var n) (var c) t) (Just c) (Just af) b (M.insert v (var n) m)
                                                            .>> compileBinds k bs ast (M.insert v (var n) m)
                                                            .>> free (glob n)
                                            _               -> Decl typ $ \n -> 
                                                                  compileProgWithName (var n, loc n) Nothing Nothing b (M.insert v (var n) m)
                                                              .>> compileBinds k bs ast (M.insert v (var n) m)

