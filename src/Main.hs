module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List
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
echoStdout handle = 
    finally (putStrLn $ \hOut ->
                runEffect $ P.fromHandle handle >-> P.toHandle hOut)
            (hClose handle) 

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage >> exit
parse ("-b":nb:cmds)     = do
                        
parse cmds   = do
  (_,mOut,mErr,procHandle) <- createProcess $ 
       (proc (head cmds) (tail cmds)) { std_out = CreatePipe
                               , std_err = CreatePipe 
                               }
  let (hOut,hErr) = maybe (error "bogus handles") 
                          id
                          ((,) <$> mOut <*> mErr)
  a1 <- async $ echoStdout hOut "stdout.txt" 
  a2 <- async $ echoStdout hErr "stderr.txt" 
  waitBoth a1 a2
  return ()
	
--   putStrLn $ "process exited: " ++ show rc
--   putStrLn $ "num bytes output: " ++ show ((length out) + (length err))
--   mapM_ putStrLn $ map ("so: " ++) $ lines out
--   mapM_ putStrLn $ map ("se: " ++) $ lines err

usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
version = putStrLn "stdout-progress 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

-- main = do
--     cmdLineArgs <- getArgs
--     (rc, out, err) <- readProcessWithExitCode (head cmdLineArgs) (tail cmdLineArgs)  ""
--     putStrLn $ "process exited: " ++ show rc
--     mapM_ putStrLn $ map ("out: " ++) $ lines out
--     mapM_ putStrLn $ map ("err: " ++) $ lines err
