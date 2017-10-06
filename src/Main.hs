module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List
import Data.IORef
import Pipes
import qualified Pipes.ByteString as P
import Pipes.Concurrent
import System.Environment
import System.Exit
import System.IO
import System.IO (hPutStrLn, stderr)
import System.Process
import System.Process

main = getArgs >>= parse 

echoStdout :: Handle -> IO ()
echoStdout handle = do
    putStrLn $ "hello"
    finally (runEffect $ P.fromHandle handle >-> P.stdout)
            (hClose handle) 

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage >> exit
parse ("-b":nb:cmds)     = putStrLn $ show nb
                        
parse cmds   = do
  (_,mOut,mErr,procHandle) <- createProcess $ 
       (proc (head cmds) (tail cmds)) { std_out = CreatePipe
                               , std_err = CreatePipe 
                               }
  let (hOut,hErr) = maybe (error "bogus handles") 
                          id
                          ((,) <$> mOut <*> mErr)
  a1 <- async $ echoStdout hOut
  a2 <- async $ echoStdout hErr
  waitBoth a1 a2
  return ()

usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
version = putStrLn "stdout-progress 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
