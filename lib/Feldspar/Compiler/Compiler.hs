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

module Feldspar.Compiler.Compiler where

import System.IO
import System.FilePath
import Data.Typeable as DT
import Control.Arrow
import Control.Applicative

import Feldspar.Core.Types
import Feldspar.Transformation
import qualified Feldspar.NameExtractor as NameExtractor
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Platforms
import Feldspar.Compiler.Backend.C.Plugin.Rule
import Feldspar.Compiler.Backend.C.Plugin.TypeDefinitionGenerator
import Feldspar.Compiler.Backend.C.Plugin.VariableRoleAssigner
import Feldspar.Compiler.Backend.C.Plugin.BlockProgramHandler
import Feldspar.Compiler.Backend.C.Plugin.TypeCorrector
import Feldspar.Compiler.Backend.C.Plugin.PrettyPrint
import Feldspar.Compiler.Backend.C.Plugin.Locator
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Imperative.Plugin.ConstantFolding
import Feldspar.Compiler.Imperative.Plugin.Free
import Feldspar.Compiler.Imperative.Plugin.IVars
import Feldspar.Compiler.Imperative.Plugin.Naming
import Feldspar.Compiler.Imperative.Plugin.Unroll

data SomeCompilable = forall a internal . Compilable a internal => SomeCompilable a
    deriving (DT.Typeable)

type Position = (Int, Int)

data SplitModuleDescriptor = SplitModuleDescriptor {
    smdSource :: Module (),
    smdHeader :: Module ()
}

data CompToCCoreResult = CompToCCoreResult {
    sourceCode      :: String,
    endPosition     :: Position,
    debugModule     :: Module DebugToCSemanticInfo
}

data SplitCompToCCoreResult = SplitCompToCCoreResult {
    sctccrSource :: CompToCCoreResult,
    sctccrHeader :: CompToCCoreResult
}

data IncludesNeeded = IncludesNeeded | NoIncludesNeeded { incneedLineNum :: Int }

moduleSplitter :: Module () -> SplitModuleDescriptor
moduleSplitter m = SplitModuleDescriptor {
    smdHeader = Module ((filter belongsToHeader $ entities m) ++ (createProcDecls $ entities m)) (moduleLabel m),
    smdSource = Module (filter (not . belongsToHeader) $ entities m) (moduleLabel m)
} where
    belongsToHeader :: Entity () -> Bool
    belongsToHeader (StructDef _ _ _ _) = True
    belongsToHeader (ProcDecl _ _ _ _ _) = True
    belongsToHeader _ = False
    createProcDecls :: [Entity ()] -> [Entity ()]
    createProcDecls [] = []
    createProcDecls (e:es) = convertProcDefToProcDecl e ++ createProcDecls es
    convertProcDefToProcDecl :: Entity () -> [Entity ()]
    convertProcDefToProcDecl e = case e of
        ProcDef name inparams outparams body label1 label2 -> [ProcDecl name inparams outparams label1 label2]
        anythingelse -> []

separateAndCompileToCCore :: (Compilable t internal)
  => (Module ()
  -> [Module ()])
  -> CompilationMode -> t -> IncludesNeeded
  -> NameExtractor.OriginalFunctionSignature -> Options
  -> [(CompToCCoreResult, Module ())]
separateAndCompileToCCore
  moduleSeparator
  compilationMode prg needed
  functionSignature coreOptions =
    pack <$> separatedModules
      where
        pack = compToCWithInfo &&& id

        separatedModules =
          moduleSeparator $
          executePluginChain' compilationMode prg functionSignature coreOptions

        compToCWithInfo = moduleToCCore needed coreOptions

moduleToCCore
  :: IncludesNeeded -> Options -> Module ()
  -> CompToCCoreResult
moduleToCCore needed opts mdl =
  CompToCCoreResult {
    sourceCode      = includes ++ moduleSrc
  , endPosition     = endPos
  , debugModule     = dbgModule
  }
  where
    (includes, lineNum) = genInclude needed

    (dbgModule, (moduleSrc, endPos)) =
      compToCWithInfos ((opts,Declaration_pl), lineNum) mdl

    genInclude IncludesNeeded         = genIncludeLines opts Nothing
    genInclude (NoIncludesNeeded ln)  = ("", ln)

-- | Compiler core
-- This functionality should not be duplicated. Instead, everything should call this and only do a trivial interface adaptation.
compileToCCore
  :: (Compilable t internal) => CompilationMode -> t -> Maybe String -> IncludesNeeded
  -> NameExtractor.OriginalFunctionSignature -> Options
  -> SplitCompToCCoreResult
compileToCCore compilationMode prg outputFileName includesNeeded
  originalFunctionSignature coreOptions =
    createSplit $ fst <$> separateAndCompileToCCore headerAndSource
      compilationMode prg includesNeeded originalFunctionSignature coreOptions
  where
    headerAndSource modules = [header, source]
      where (SplitModuleDescriptor header source) = moduleSplitter modules

    createSplit [header, source] = SplitCompToCCoreResult header source

genIncludeLinesCore :: [String] -> (String, Int)
genIncludeLinesCore []   = ("", 1)
genIncludeLinesCore (x:xs) = ("#include " ++ x ++ "\n" ++ str, linenum + 1) where
    (str, linenum) = genIncludeLinesCore xs

genIncludeLines :: Options -> Maybe String -> (String, Int)
genIncludeLines coreOptions mainHeader = (str ++ "\n\n", linenum + 2) where
    (str, linenum)  = genIncludeLinesCore $ (includes $ platform coreOptions) ++ mainHeaderCore
    mainHeaderCore = case mainHeader of
        Nothing -> []
        Just filename -> ["\"" ++ takeFileName filename ++ ".h\""]

-- | Predefined options

defaultOptions
    = Options
    { platform          = c99
    , unroll            = NoUnroll
    , debug             = NoDebug
    , memoryInfoVisible = True
    , rules             = []
    }

c99PlatformOptions              = defaultOptions
tic64xPlatformOptions           = defaultOptions { platform = tic64x }
unrollOptions                   = defaultOptions { unroll = Unroll 8 }
noPrimitiveInstructionHandling  = defaultOptions { debug = NoPrimitiveInstructionHandling }
noMemoryInformation             = defaultOptions { memoryInfoVisible = False }

-- | Plugin system

pluginChain :: ExternalInfoCollection -> Module () -> Module ()
pluginChain externalInfo
    = (executePlugin RulePlugin (ruleExternalInfo externalInfo))
    . (executePlugin TypeDefinitionGenerator (typeDefinitionGeneratorExternalInfo externalInfo))
    . (executePlugin ConstantFolding ())
    . (executePlugin UnrollPlugin (unrollExternalInfo externalInfo))
    . (executePlugin Precompilation (precompilationExternalInfo externalInfo))
    . (executePlugin RulePlugin (primitivesExternalInfo externalInfo))
    . (executePlugin Free ())
    . (executePlugin IVarPlugin ())
    . (executePlugin VariableRoleAssigner (variableRoleAssignerExternalInfo externalInfo))
    . (executePlugin TypeCorrector (typeCorrectorExternalInfo externalInfo))
    . (executePlugin BlockProgramHandler ())

data ExternalInfoCollection = ExternalInfoCollection {
      precompilationExternalInfo          :: ExternalInfo Precompilation
    , unrollExternalInfo                  :: ExternalInfo UnrollPlugin
    , primitivesExternalInfo              :: ExternalInfo RulePlugin
    , ruleExternalInfo                    :: ExternalInfo RulePlugin
    , typeDefinitionGeneratorExternalInfo :: ExternalInfo TypeDefinitionGenerator
    , variableRoleAssignerExternalInfo    :: ExternalInfo VariableRoleAssigner
    , typeCorrectorExternalInfo           :: ExternalInfo TypeCorrector
}

executePluginChain' :: (Compilable c internal)
  => CompilationMode -> c -> NameExtractor.OriginalFunctionSignature
  -> Options -> Module ()
executePluginChain' compilationMode prg originalFunctionSignatureParam opt =
  pluginChain ExternalInfoCollection {
    precompilationExternalInfo = PrecompilationExternalInfo {
        originalFunctionSignature = fixedOriginalFunctionSignature
      , inputParametersDescriptor = buildInParamDescriptor prg
      , numberOfFunctionArguments = numArgs prg
      , compilationMode           = compilationMode
      }
    , unrollExternalInfo                  = unroll opt
    , primitivesExternalInfo              = opt{ rules = platformRules $ platform opt }
    , ruleExternalInfo                    = opt
    , typeDefinitionGeneratorExternalInfo = opt
    , variableRoleAssignerExternalInfo    = ()
    , typeCorrectorExternalInfo           = False
    } $ fromCore "PLACEHOLDER" prg
  where
    ofn = NameExtractor.originalFunctionName
    errorPEI =
      (error "There is no defaultArraySize any more", debug opt, platform opt)
    fixedOriginalFunctionSignature = originalFunctionSignatureParam {
      NameExtractor.originalFunctionName =
        fixFunctionName $ ofn originalFunctionSignatureParam
    }

executePluginChain cm f sig opts =
  moduleSplitter $ executePluginChain' cm f sig opts