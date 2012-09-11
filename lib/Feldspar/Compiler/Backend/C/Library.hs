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

module Feldspar.Compiler.Backend.C.Library
    (module System.Console.ANSI,
     module Feldspar.Compiler.Backend.C.Library) where

import Control.Monad.State
import System.Console.ANSI
import System.FilePath
import qualified Feldspar.Compiler.Imperative.Representation as AIR

data CompilationMode = Interactive | Standalone
    deriving (Show, Eq)

type AllocationInfo = ([AIR.Type],[AIR.Type],[AIR.Type])
    
-- ===========================================================================
--  == String tools
-- ===========================================================================

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl | take (length find) s == find = repl ++ replace (drop (length find) s) find repl
                    | otherwise = head s : replace (tail s) find repl

fixFunctionName :: String -> String
fixFunctionName functionName = replace (replace functionName "_" "__") "'" "_prime"

makeDebugHFileName :: String -> String
makeDebugHFileName = (<.> "h.dbg.txt")

makeDebugCFileName :: String -> String
makeDebugCFileName = (<.> "c.dbg.txt")

makeHFileName :: String -> String
makeHFileName = (<.> "h")

makeCFileName :: String -> String
makeCFileName = (<.> "c")

-- ===========================================================================
--  == Name generator
-- ===========================================================================

newName  :: (Monad m) => String -> StateT Integer m String
newName name = do
    n <- get
    put $ n+1
    return $ name ++ show n

-- ===========================================================================
--  == Console tools
-- ===========================================================================

withColor :: Color -> IO () -> IO ()
withColor color action = do
    setSGR [SetColor Foreground Vivid color, SetColor Background Dull Black] -- , SetConsoleIntensity BoldIntensity]
    action
    setSGR [Reset]
