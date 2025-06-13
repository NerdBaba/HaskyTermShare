module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll) -- Keep for later network integration
import qualified Data.ByteString.Char8 as B
import System.Posix.Pty (Pty, spawnWithPty, readPty, writePty, closePty) -- Import PTY functions
import System.Process (waitForProcess)
import System.Posix.Types (ProcessID)
import System.IO (stdin, hIsEOF) -- Keep necessary IO functions, add hIsEOF for stdin
import Control.Concurrent (threadDelay) -- Only need threadDelay now
import System.Posix.Terminal -- Import terminal control functions and types
import Control.Monad (forever) -- Keep forever
import System.Posix.Signals -- Import signal handling functions
import Control.Concurrent.Async (async, cancel, withAsync) -- Need async, cancel, withAsync
import System.Posix.IO -- Import handleToFd
import Control.Exception (finally)
import Control.Monad.IO.Class ( MonadIO ) -- Import MonadIO

main :: IO ()
main = withSocketsDo $ do
    -- Ignore SIGINT (Ctrl+C) for now; we'll handle exit later
    installHandler sigINT Ignore Nothing

    -- Get file descriptor for stdin
    stdinFd <- handleToFd stdin
    -- Get current terminal settings
    originalTermSettings <- getTerminalAttributes stdinFd
    -- Set terminal to cbreak mode to receive input character by character
    let cbreakTermSettings = originalTermSettings `withoutMode` ProcessInput `withoutMode` EnableEcho -- Apply changes here

    -- Use finally to ensure original terminal settings are restored on exit
    finally (do
        setTerminalAttributes stdinFd cbreakTermSettings Immediately

        -- Original network connection part (will integrate PTY output here later)
        addr <- resolve "127.0.0.1" "3000"
        sock <- open addr
        putStrLn "Connected to server on port 3000"
        -- sendAll sock (B.pack "Hello from the client!") -- Not needed anymore

        putStrLn "Spawning PTY..."

        -- Create a PTY and fork a process (e.g., /bin/bash)
        -- The second argument 'True' makes the spawned process the session leader
        (pty, pid) <- spawnWithPty Nothing True "/bin/bash" [] (80, 24) -- Capture pid

        putStrLn "PTY spawned. Reading output and writing input..."

        -- Use withAsync to handle the PTY reading concurrently
        -- Pass the socket to readPtyOutput
        withAsync (readPtyOutput sock pty) $ \ptyReaderAsync ->
          -- Use withAsync for the PTY writing concurrently (reading from stdin)
          withAsync (writePtyInput pty) $ \ptyWriterAsync ->
            -- Use withAsync for reading input from the server socket and writing to the PTY
            withAsync (readServerInputAndWriteToPty sock pty) $ \serverInputAsync -> do

                putStrLn "PTY reader, writer, and server input handler threads started. Waiting for PTY process to exit..."
                -- Wait for the process running in the PTY to exit
                exitCode <- waitForProcess pid
                putStrLn $ "PTY process exited with exit code: " ++ show exitCode

                -- Cancel the async tasks once the process exits
                -- This is important to clean up the reading, writing, and server input threads
                -- Also close the PTY and the socket
                cancel ptyReaderAsync
                cancel ptyWriterAsync
                cancel serverInputAsync -- Cancel the new server input task
                closePty pty -- Close the PTY handle using closePty
                close sock -- Close the network socket

        -- sendAll sock (B.pack "Client finished.") -- Will add proper session termination later
        -- close sock -- Will add proper session termination later
        putStrLn "Client shutting down." -- Removed the newline character
        ) (do
            -- Restore original terminal settings
            setTerminalAttributes stdinFd originalTermSettings Immediately
            putStrLn "Original terminal settings restored." -- Removed the newline character
        )

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
    putStrLn "writePtyInput: Waiting for input from stdin..." -- Added logging
    input <- B.hGetSome stdin 1024
    putStrLn $ "writePtyInput: Read " ++ show (B.length input) ++ " bytes from stdin." -- Added logging
    if not (B.null input)
        then do
            writePty pty input -- Write to PTY using writePty
            -- No need for hFlush with writePty
        else do
             -- If input is empty, for interactive stdin, this might indicate EOF (Ctrl+D)
             -- Let's add a check for EOF on stdin to exit this loop gracefully
             eof <- hIsEOF stdin
             if eof
                 then do
                     putStrLn "Stdin closed. Stopping input writer."
                     return () -- Exit forever loop
                 else
                     -- Otherwise, no input read, just continue
                     return ()

-- Reads input from the server socket and writes it to the PTY
readServerInputAndWriteToPty :: Socket -> Pty -> IO ()
readServerInputAndWriteToPty sock pty = forever $ do
    -- Read data from the server socket (viewer input)
    msg <- recv sock 1024
    if B.null msg
        then do
            -- Server closed the connection (recv returned empty ByteString)
            putStrLn "\nServer connection closed. Stopping server input handler."
            return () -- Exit the forever loop
        else do
            -- Received data from server, write it to the PTY input
            -- putStrLn $ "Received " ++ show (B.length msg) ++ " bytes from server. Writing to PTY." -- Optional: for debugging
            writePty pty msg -- Write to PTY using writePty

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
