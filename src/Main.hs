{-# LANGUAGE OverloadedStrings #-}
module Main where

--import           Control.Applicative      ((*>))
--import           Control.Concurrent.Async (Concurrently (..))
--import           Data.Conduit             (await, yield, ($$), (=$))
--import qualified Data.Conduit.Binary      as CB
--import qualified Data.Conduit.List        as CL
--import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
--                                           proc, waitForStreamingProcess)
import           System.IO                (stdin, stdout)
import           System.Environment
import           System.Exit
--
--import           ClassyPrelude.Conduit

import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.Foldable as F

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

-- | An example that produces indexed output.
testSource :: (Monad m) => Source m (String)
testSource = do
    yield "test"
    yield "what"

main :: IO ()
main = do
    let countLoop = do
            mbs <- await
            case mbs of
                Nothing -> do
                    yield (0, "yo")
                Just bs -> do
                    yield (0, bs)
                    yield (1, bs)
                    yield (2, bs)
                    countLoop

    testSource
      $$ countLoop
      =$ multiSink_ (map testSink ["1: ", "2: ", "3: "])
-- main = getArgs >>= parse 
-- 
-- parse ["-h"] = usage   >> exit
-- parse ["-v"] = version >> exit
-- parse []     = usage >> exit
-- parse ("-b":nb:cmds)     = putStrLn $ show nb
--                         
-- parse cmds   = do
--     ((toProcess, close), fromProcess, fromStderr, cph) <-
--         streamingProcess (proc "ls" [])
-- --        streamingProcess (proc (head cmds) (tail cmds))
-- 
--     let input = CB.sourceHandle stdin
--              $$ CB.lines
--              =$ inputLoop
--              =$ toProcess
-- 
--         inputLoop = do
--             close
-- --            mbs <- await
-- --            case mbs of
-- --                Nothing -> close
-- --                Just "quit" -> close
-- --                Just bs -> do
-- --                    yield bs
-- --                    inputLoop
-- 
-- --        output = fromProcess $$ CL.mapM_
-- --            (\bs -> putStrLn $ "from process: " ++ show bs)
--         output = fromProcess
--             $$ CB.lines
--             =$ countLoop
--             =$ CB.sinkHandle stdout
-- 
--         countLoop = do
--             mbs <- await
--             case mbs of
--                 Nothing -> do
--                     yield "yo"
--                     yield "\n"
--                 Just bs -> do
--                     yield bs
--                     yield "\n"
--                     countLoop
-- 
--         errout = fromStderr $$ CL.mapM_
--             (\bs -> putStrLn $ "from stderr: " ++ show bs)
-- 
--     ec <- runConcurrently $
--         Concurrently input *>
--         Concurrently output *>
--         Concurrently errout *>
--         Concurrently (waitForStreamingProcess cph)
-- 
--     putStrLn $ "Process exit code: " ++ show ec
-- 
-- usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
-- version = putStrLn "stdout-progress 0.1"
-- exit    = exitWith ExitSuccess
-- die     = exitWith (ExitFailure 1)
