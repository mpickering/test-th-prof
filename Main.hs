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

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

prog :: TH.Q [TH.Dec]
prog =
    do TH.runIO (print $ fib 40)
       return []

initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    df1 <- getSessionDynFlags
    let cmdOpts = ["-package", "ghc", "-fforce-recomp"] ++ xs
    (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
    -- pprTraceM "leftovers" $ ppr leftovers
    setSessionDynFlags df2
    ts <- mapM (flip G.guessTarget Nothing) $ map unLoc leftovers
    -- (df2, ts)

    setTargets ts

    -- mod_graph <- G.depanal [] True
    void $ G.load LoadAllTargets -- Nothing mod_graph
    hsc_env <- getSession
    let n = mkFastString "A"
    liftIO $ initTc hsc_env HsSrcFile False (mkBaseModule n) undefined (runQuasi prog)

    -- df <- getSessionDynFlags

    return ()

main :: IO ()
main = do
    xs <- getArgs
    runGhc (Just libdir) $ initGhcM xs
