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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Feldspar.Compiler.Backend.C.CodeGeneration where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Error (handleError, ErrorClass(..))
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library

import Feldspar.Range (isSingleton, upperBound, Range(..))
import Feldspar.Core.Types (Length)

import Data.List (intercalate)

-- =======================
-- == C code generation ==
-- =======================

codeGenerationError :: ErrorClass -> String -> a
codeGenerationError = handleError "CodeGeneration"

class ToC a where
    toC :: Options -> Place -> a -> String

getStructTypeName :: Options -> Place -> Type -> String
getStructTypeName options place (StructType ts) =
    intercalate "_" $ map (getStructTypeName options place . snd) ts
getStructTypeName options place (ArrayType len innerType) =
    "arr_T" ++ getStructTypeName options place innerType ++ "_S" ++ len2str len
    where
        len2str :: Range Length -> String
        len2str r | isSingleton r = show (upperBound r)
                  | otherwise = "UD"
getStructTypeName options place t = replace (toC options place t) " " "" -- float complex -> floatcomplex

instance ToC Type where
    toC _ _ VoidType                = "void"
    toC _ _ ArrayType{}             = arrayTypeName
    toC _ _ IVarType{}              = ivarTypeName
    toC _ _ (UserType u)            = u
    toC o p (NativeArray _ t)       = toC o p t
    toC o p t@(StructType _)        = "struct s_" ++ getStructTypeName o p t
    toC o _ t | [s] <- [s | (t',s,_) <- types $ platform o, t'==t] = s
    toC o p t = codeGenerationError InternalError
              $ unwords ["Unhandled type in platform ", name (platform o),  ": ", show t, " place: ", show p]

arrayTypeName :: String
arrayTypeName = "struct array"

ivarTypeName :: String
ivarTypeName = "struct ivar"

instance ToC (Variable ()) where
    toC options place Variable{..} = showVariable options place varRole varType varName

showVariable :: Options -> Place -> VariableRole -> Type -> String -> String
showVariable options place role typ vname = var ++ sz where
    var = listprint id " " [variableType, showName role place typ vname]
    variableType = showType options role place typ restr
    restr
        | place == MainParameter_pl = isRestrict $ platform options
        | otherwise = NoRestrict
    sz = case place of
           MainParameter_pl -> variableSize typ
           Declaration_pl   -> variableSize typ
           _                -> ""
    variableSize (NativeArray l t) = "[" ++ (maybe "" show l) ++ "]" ++ variableSize t
    variableSize _                 = ""

showType :: Options -> VariableRole -> Place -> Type -> IsRestrict -> String
showType options role MainParameter_pl (NativeArray _ t) res = showType options role Declaration_pl t res
showType options role MainParameter_pl t _
    | passByReference t || role == Pointer  = tname ++ " *"
    | otherwise                             = tname
  where
    tname = toC options MainParameter_pl t
showType options _ Declaration_pl t _ = toC options Declaration_pl t
showType _ _ _ _ _ = ""

showName :: VariableRole -> Place -> Type -> String -> String
showName Value place t n
    | place == AddressNeed_pl = '&' : n
    | place == FunctionCallIn_pl && passByReference t  = '&' : n
showName Pointer _ ArrayType{} n   = n
showName Pointer _ NativeArray{} n = n
showName Pointer place _ n
    | place == AddressNeed_pl      = n
    | place == Declaration_pl      = '*' : n
    | place == MainParameter_pl    = n
    | otherwise = "(* " ++ n ++ ")"
showName _ _ _ n = n

passByReference :: Type -> Bool
passByReference ArrayType{}  = True
passByReference StructType{} = True
passByReference _            = False

----------------------
-- Helper functions --
----------------------

listprint :: (a->String) -> String -> [a] -> String
listprint f s = intercalate s . filter (not . null) . map f
