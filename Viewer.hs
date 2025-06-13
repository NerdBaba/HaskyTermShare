module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as B
import Control.Monad (forever)
import System.IO (stdout, hFlush, stdin, hIsEOF)
import Control.Concurrent.Async (withAsync, async, cancel)
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import System.Posix.Signals -- Import signal handling functions

import System.Posix.IO -- Import handleToFd
import System.Posix.Types (Fd) -- Import Fd type
import Control.Monad.IO.Class (MonadIO) -- Import MonadIO
import System.Posix.Terminal ( getTerminalAttributes, setTerminalAttributes, TerminalAttributes, TerminalMode(ProcessInput, EnableEcho), TerminalState(..), withoutMode ) -- Import necessary terminal control elements


main :: IO ()
main = withSocketsDo $ do
    -- Ignore SIGINT (Ctrl+C) for now; we'll handle exit later
    _ <- installHandler sigINT Ignore Nothing
    -- Get file descriptor for stdin
    stdinFd <- handleToFd stdin
    -- Get current terminal settings
    originalTermSettings <- getTerminalAttributes stdinFd
    -- Set terminal to cbreak mode
    let cbreakTermSettings = originalTermSettings `withoutMode` ProcessInput `withoutMode` EnableEcho

    -- Use finally to ensure original terminal settings are restored on exit
    finally
      (do
        -- Set new terminal settings
        setTerminalAttributes stdinFd cbreakTermSettings Immediately

        -- Original main logic starts here
        putStrLn "Viewer connecting to server on port 3000..."
        addr <- resolve "127.0.0.1" "3000"
        sock <- open addr
        putStrLn "Connected. Waiting for terminal data and sending input..."

        -- Use withAsync to run receiving and sending concurrently
        withAsync (handleServerConnection sock) $ \receiverAsync ->
          withAsync (handleViewerInput sock) $ \senderAsync -> do
            putStrLn "Viewer threads started. Press Ctrl+C to exit."

            -- Wait indefinitely or until one of the threads finishes (e.g., due to disconnect)
            -- A better approach would be to wait for disconnect signals from either side.
            -- For now, just wait for asyncs to complete (will only happen on error/disconnect).
            -- Or we could wait for the main thread to receive an interrupt.
            -- For this basic step, let's just keep the main thread alive until an exception occurs
            -- or the asyncs somehow finish. Using a simple forever loop with a delay for now.
            -- In a real application, you'd want more robust shutdown handling.
            forever $ threadDelay 1000000 -- Keep main thread alive

        putStrLn "vViewer shutting down."
        close sock
      )
      (do
        -- Restore original terminal settings
        setTerminalAttributes stdinFd originalTermSettings Immediately
        putStrLn "Original terminal settings restored."
      )

handleServerConnection :: Socket -> IO ()
handleServerConnection sock =
    -- Ensure the socket is closed if this thread exits unexpectedly
    finally
        (forever $ do
            msg <- recv sock 1024
            if B.null msg
                then do
                    putStrLn "\nServer disconnected."
                    return () -- Exit the forever loop
                else do
                    B.putStr msg
                    hFlush stdout
        )
        (return ()) -- Temporarily simplified cleanup action

-- Function to handle input from the viewer's stdin and send to the server
handleViewerInput :: Socket -> IO ()
handleViewerInput sock =
    -- Ensure the socket is closed if this thread exits unexpectedly
    finally
        (forever $ do
            -- Read some bytes from stdin (blocking until input is available)
            putStrLn "handleViewerInput: Waiting for input from stdin..." -- Added logging
            input <- B.hGetSome stdin 1024
            putStrLn $ "handleViewerInput: Read " ++ show (B.length input) ++ " bytes from stdin." -- Added logging
            if not (B.null input)
                then do
                    -- Send the input to the server
                    sendAll sock input
                else do
                     -- If input is empty, for interactive stdin, this might indicate EOF (Ctrl+D)
                     eof <- hIsEOF stdin
                     if eof
                         then do
                             putStrLn "Stdin closed. Stopping input sender."
                             return () -- Exit forever loop
                         else
                             -- Otherwise, no input read, just continue
                             return ()
        )
        (return ()) -- Temporarily simplified cleanup action


resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
