{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Binary      as CB
import qualified Data.Foldable as F
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin, stdout)
import           System.Environment
import           System.Exit

-- | Filter only pairs tagged with the appropriate key.
filterInputC :: (Monad m, Eq k) => k -> Conduit (k, a) m a
filterInputC idx = C.filter ((idx ==) . fst) =$= C.map snd

-- | Prepend a given sink with a filter.
filterInput :: (Monad m, Eq k) => k -> Sink a m r -> Sink (k, a) m r
filterInput idx = (filterInputC idx =$)

-- | Given a list of sinks, create a single sink that directs received values
-- depending on the index.
multiSink_ :: (Monad m) => [Sink a m ()] -> Sink (Int, a) m ()
multiSink_ = getZipSink . F.sequenceA_ . fmap ZipSink
             . zipWith filterInput [0..]

-- | A testing sink that just prints its input, marking it with
-- a given prefix.
testSink :: String -> Sink String IO ()
testSink prefix = C.mapM_ (putStrLn . (prefix ++))

lengthMap :: (Monad m) => Conduit String m Int
lengthMap = C.map length

lengthSum :: (Monad m) => Conduit Int m String
lengthSum = do
    let loop currSum = do
            item <- await
            case item of
                Nothing -> yield ("Total Bytes: " ++ (show currSum))
                Just val -> do
                    loop $ currSum + val
    loop 0 

lengthSink :: Sink String IO ()
lengthSink = C.mapM_ putStrLn

main = getArgs >>= parse 

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage >> exit
parse ("-b":nb:cmds)     = putStrLn $ show nb
                        
parse cmds   = do
    ((toProcess, close), fromProcess, fromStderr, cph) <-
        streamingProcess (proc "ls" [])
--        streamingProcess (proc (head cmds) (tail cmds))

    let input = CB.sourceHandle stdin
             $$ CB.lines
             =$ inputLoop
             =$ toProcess

        inputLoop = do
            close
        output = fromProcess
            $$ splitLoop =$ multiSink_ [(testSink "orig:"), (lengthMap $= lengthSum $= lengthSink)]

        splitLoop = do
            mbs <- await
            case mbs of
                Nothing -> return ()
                Just bs -> do
                    yield (0, bs)
                    yield (1, bs)
                    splitLoop

        errout = fromStderr $$ C.mapM_
            (\bs -> putStrLn $ "from stderr: " ++ show bs)

    ec <- runConcurrently $
        Concurrently input *>
        Concurrently output *>
        Concurrently errout *>
        Concurrently (waitForStreamingProcess cph)

    putStrLn $ "Process exit code: " ++ show ec

usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
version = putStrLn "stdout-progress 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
