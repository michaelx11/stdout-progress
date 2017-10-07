{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, ($$), (=$))
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin, stdout)
import           System.Environment
import           System.Exit

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
--            mbs <- await
--            case mbs of
--                Nothing -> close
--                Just "quit" -> close
--                Just bs -> do
--                    yield bs
--                    inputLoop

--        output = fromProcess $$ CL.mapM_
--            (\bs -> putStrLn $ "from process: " ++ show bs)
        output = fromProcess
            $$ CB.lines
            =$ countLoop
            =$ CB.sinkHandle stdout

        countLoop = do
            mbs <- await
            case mbs of
                Nothing -> do
                    yield "yo"
                    yield "\n"
                Just bs -> do
                    yield bs
                    yield "\n"
                    countLoop

        errout = fromStderr $$ CL.mapM_
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
