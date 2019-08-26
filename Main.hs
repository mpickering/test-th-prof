module Main where

import GHC as G
import GhcMake as G
import DynFlags
import SrcLoc as G
import GHC.Paths

import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import TcSplice
import PrelNames
import TcRnMonad
import DriverPhases
import Control.Concurrent
import FastString
import HscTypes
import TcRnDriver
import Data.List

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

prog :: TH.Q [TH.Dec]
prog = do
     let loop :: TH.Q ()
         loop = do n <- TH.qLookupName True "A.A"
                   let n' = case n of
                              Just n -> n
                              Nothing -> error "ERROR"
                   i <- TH.reify n'
                   i `seq` return ()

     replicateM_ 1000000 loop
     return []

initGhcM :: [String] -> Ghc ()
initGhcM xs = do
   df1 <- getSessionDynFlags
   let cmdOpts = ["A.hs", "-fforce-recomp"] ++ xs
   (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
   -- pprTraceM "leftovers" $ ppr leftovers
   setSessionDynFlags df2
   ts <- mapM (flip G.guessTarget Nothing) $ map unLoc leftovers
   -- (df2, ts)

   setTargets ts

   -- mod_graph <- G.depanal [] True
   void $ G.load LoadAllTargets -- Nothing mod_graph

   -- Then find dependencies
   modGraph <- depanal [] True
   let fn = "A.hs"
   hsc_env <- getSession
   let n = mkFastString "A"
   let loc = mkRealSrcLoc n 0 0
       rspan = mkRealSrcSpan loc loc
   case find ((== fn) . msHsFilePath) (mgModSummaries modGraph) of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
         pmod <- parseModule modSummary
         let hpm = HsParsedModule { hpm_module = parsedSource pmod,
                                 hpm_src_files = pm_extra_src_files pmod,
                                 hpm_annotations = pm_annotations pmod }
         liftIO $ initTc hsc_env HsSrcFile False (mkBaseModule n) rspan $ do
           gbl_env <- tcRnModuleTcRnM hsc_env modSummary hpm (mkBaseModule n, RealSrcSpan rspan)
           failIfErrsM
           setGblEnv gbl_env $ runQuasi prog
     Nothing -> error "compileToCoreModule: target FilePath not found in module dependency graph"

    -- df <- getSessionDynFlags

   return ()


main :: IO ()
main = do
    xs <- getArgs
    runGhc (Just libdir) $ initGhcM xs
