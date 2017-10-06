module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens ( view )
import Control.Monad
import Data.Int
import Data.List
import Data.IORef
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as P
import qualified Data.ByteString.Lazy.Char8 as BL 
import Pipes.Concurrent
import System.Environment
import System.Exit
import System.IO
import System.IO (hPutStrLn, stderr)
import System.Process
import System.Process

main = getArgs >>= parse 

type Counter = Int64 -> IO Int64

makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> atomicModifyIORef r (\a -> (a + i, a + i)))

splitLazy :: (Monad m, Integral n) =>
   n -> Producer P.ByteString m r -> m (BL.ByteString, Producer P.ByteString m r)
splitLazy n bs = do
  (bss, rest) <- PP.toListM' $ view (P.splitAt n) bs
  return (BL.fromChunks bss, rest)

measureChunks :: Monad m => Producer P.ByteString m r -> Producer BL.ByteString m r
measureChunks bs = do
 (lbs, rest) <- lift $ splitLazy 1 bs
 if BL.length lbs /= 1
   then rest >-> PP.drain -- in fact it will be empty
   else do
--     let w32 = G.runGet G.getWord32be lbs
--     (lbs', rest') <- lift $ splitLazy w32 bs
--     counter (BL.length lbs)
     Pipes.yield lbs
     measureChunks rest

echoStdout :: Handle -> Counter -> IO ()
echoStdout handle counter = do
    putStrLn $ "hello"
    runEffect $ (measureChunks (P.fromHandle handle))  >-> PP.print
--    runEffect $ (countBytes (P.fromHandle handle) counter)  >-> P.stdout
--    n <- P.length $ runEffect $ P.fromHandle handle >-> P.stdout
    hClose handle
--    putStrLn $ show 

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage >> exit
parse ("-b":nb:cmds)     = putStrLn $ show nb
                        
parse cmds   = do
    byteCounter <- makeCounter
    (_,mOut,mErr,procHandle) <- createProcess $ 
         (proc (head cmds) (tail cmds)) { std_out = CreatePipe
                                 , std_err = CreatePipe 
                                 }
    let (hOut,hErr) = maybe (error "bogus handles") 
                            id
                            ((,) <$> mOut <*> mErr)
    a1 <- async $ echoStdout hOut byteCounter
    a2 <- async $ echoStdout hErr byteCounter
    waitBoth a1 a2
    count <- byteCounter 0
    print count
    return ()

usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
version = putStrLn "stdout-progress 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
