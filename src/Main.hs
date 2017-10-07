{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             as C
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import            Data.ByteString.Internal
import           Data.ByteString.Char8    (unpack)
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import qualified Data.Foldable            as F
import           System.IO                (stdin, stdout)
import           System.Environment
import           System.Exit
import           Text.Printf
import           Text.Read

main :: IO ()
main = getArgs >>= parse

-- | Filter only pairs tagged with the appropriate key.
filterInputC :: (Monad m, Eq k) => k -> Conduit (k, a) m a
filterInputC idx = CL.filter ((idx ==) . fst) =$= CL.map snd

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
testSink prefix = CL.mapM_ (putStrLn . (prefix ++))

lengthMap :: (Monad m) => Conduit String m Int
lengthMap = CL.map length

lengthSum :: (Monad m) => Conduit Int m String
lengthSum = do
    let loop currSum = do
            item <- await
            case item of
                Nothing -> yield ("\n\nTotal Bytes: " ++ (show currSum))
                Just val -> do
                    loop $ currSum + val
    loop 0 

lengthSink :: Sink String IO ()
lengthSink = CL.mapM_ putStrLn

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = usage >> exit
parse ("-b":nb:cmds)     = do
    let maybeInt = readMaybe nb :: Maybe Int
    case maybeInt of
        Nothing -> putStrLn ("Invalid byte count: " ++ nb) >> exit
        Just numBytes  -> do
            ((toProcess, close), fromProcess, fromStderr, cph) <-
                streamingProcess (proc (head cmds) (tail cmds))
     
            let input = CB.sourceHandle stdin
                     $$ CB.lines
                     =$ inputLoop
                     =$ toProcess
     
                inputLoop = do
                    close
     
                output = fromProcess
                    $$ CB.lines
                    =$ CL.map unpack
                    =$ progressLoop 0
                    =$ CL.mapM_ (\bs -> (printf "%s" bs))

     
                progressLoop currentBytes = do
                    line <- await
                    case line of
                        Nothing -> yield "\n"
                        Just actualLine -> do
--                            yield actualLine
                            yield $ "\r" ++ (show ((quot currentBytes numBytes) * 100)) ++ " %"
--                            yield $ actualLine ++ "\n"
                            progressLoop $ ((length actualLine) + currentBytes)
     
                errout = fromStderr $$ CL.mapM_
                    (\bs -> putStrLn $ "stderr: " ++ show bs)
     
            ec <- runConcurrently $
                Concurrently input *>
                Concurrently output *>
                Concurrently errout *>
                Concurrently (waitForStreamingProcess cph)
     
            return ()
                        
parse cmds   = do
    ((toProcess, close), fromProcess, fromStderr, cph) <-
        streamingProcess (proc (head cmds) (tail cmds))

    let input = CB.sourceHandle stdin
             $$ CB.lines
             =$ inputLoop
             =$ toProcess

        inputLoop = do
            close

        output = fromProcess
            $$ CB.lines
            =$ CL.map unpack
            =$ splitLoop
            =$ multiSink_ [(testSink "stdout: "), (lengthMap $= lengthSum $= lengthSink)]

        splitLoop = do
            mbs <- await
            case mbs of
                Nothing -> return ()
                Just bs -> do
                    yield (0, bs)
                    yield (1, bs)
                    splitLoop

        errout = fromStderr $$ CL.mapM_
            (\bs -> putStrLn $ "stderr: " ++ show bs)

    ec <- runConcurrently $
        Concurrently input *>
        Concurrently output *>
        Concurrently errout *>
        Concurrently (waitForStreamingProcess cph)

    return ()

usage   = putStrLn "Usage: stdout-progress [-vh] [-b num bytes] [cmd ... args]"
version = putStrLn "stdout-progress 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
