module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll) -- Keep for later network integration
import qualified Data.ByteString.Char8 as B
import System.Posix.Pty (Pty, spawnWithPty, readPty, writePty, closePty) -- Import PTY functions
import System.Process (waitForProcess)
import System.Posix.Types (ProcessID)
import System.IO (stdin, hIsEOF) -- Keep necessary IO functions, add hIsEOF for stdin
import Control.Concurrent (threadDelay) -- Only need threadDelay now
import Control.Monad (forever) -- Keep forever
import Control.Concurrent.Async (async, cancel, withAsync) -- Need async, cancel, withAsync


main :: IO ()
main = withSocketsDo $ do
    -- Original network connection part (will integrate PTY output here later)
    addr <- resolve "127.0.0.1" "3000"
    sock <- open addr
    putStrLn "Connected to server on port 3000"
    -- sendAll sock (B.pack "Hello from the client!") -- Not needed anymore

    putStrLn "Spawning PTY..."
    -- Create a PTY and fork a process (e.g., /bin/bash)
    -- The second argument \'True\' makes the spawned process the session leader
    (pty, pid) <- spawnWithPty Nothing True "/bin/bash" [] (80, 24) -- Capture pid

    putStrLn "PTY spawned. Reading output and writing input..."

    -- Use withAsync to handle the PTY reading concurrently
    -- Pass the socket to readPtyOutput
    withAsync (readPtyOutput sock pty) $ \ptyReaderAsync ->
      -- Use withAsync for the PTY writing concurrently (reading from stdin)
      withAsync (writePtyInput pty) $ \ptyWriterAsync -> do
        putStrLn "PTY reader and writer threads started. Waiting for PTY process to exit..."
        -- Wait for the process running in the PTY to exit
        exitCode <- waitForProcess pid
        putStrLn $ "PTY process exited with exit code: " ++ show exitCode

        -- Cancel the async tasks once the process exits
        -- This is important to clean up the reading and writing threads
        -- Also close the PTY and the socket
        cancel ptyReaderAsync
        cancel ptyWriterAsync
        closePty pty -- Close the PTY handle using closePty
        close sock -- Close the network socket

    -- sendAll sock (B.pack "Client finished.") -- Will add proper session termination later
    -- close sock -- Will add proper session termination later
    putStrLn "Client shutting down."

-- Modified to accept a Socket and send output over the network
readPtyOutput :: Socket -> Pty -> IO ()
readPtyOutput sock pty = do
    -- Continuously read output from the PTY handle
    output <- readPty pty -- Use readPty
    if B.null output
        then do
            -- An empty ByteString from readPty indicates the PTY is closed (process exited)
            putStrLn "\nPTY output stream closed."
            return () -- Stop reading
        else do
            -- Send the output over the network socket
            sendAll sock output
            B.putStr output -- Display output locally
            readPtyOutput sock pty -- Continue reading

writePtyInput :: Pty -> IO ()
writePtyInput pty = forever $ do
    -- Read some bytes from stdin (blocking until input is available)
    -- This is a basic approach; a real terminal would handle raw input, line editing, etc.
    input <- B.hGetSome stdin 1024
    if not (B.null input)
        then do
            writePty pty input -- Write to PTY using writePty
            -- No need for hFlush with writePty
        else do
             -- If input is empty, for interactive stdin, this might indicate EOF (Ctrl+D)
             -- Let\'s add a check for EOF on stdin to exit this loop gracefully
             eof <- hIsEOF stdin
             if eof
                 then do
                     putStrLn "Stdin closed. Stopping input writer."
                     return () -- Exit forever loop
                 else
                     -- Otherwise, no input read, just continue
                     return ()


-- Keeping resolve and open functions for later network integration
resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

-- threadDelay is imported from Control.Concurrent
-- threadDelay :: Int -> IO ()
-- threadDelay microseconds = Control.Concurrent.threadDelay microseconds -- Removed
