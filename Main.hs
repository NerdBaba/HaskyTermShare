{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.Socket hiding (recv) -- Hide recv from Network.Socket to avoid ambiguity
import Network.Socket.ByteString (recv, sendAll) -- Import recv and sendAll for ByteString
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.MVar -- Import MVar
import Control.Exception (finally, handle, IOException) -- Import exception handling
import Data.List (delete) -- Import delete for list manipulation

-- Define the server state type
type ServerState = MVar (Maybe Socket, [Socket])

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "3000"
    sock <- open addr
    putStrLn $ "Socket opened: " ++ show sock -- Added logging
    listen sock 1
    putStrLn "Server listening on port 3000"
    putStrLn "Waiting for incoming connections..." -- Added logging

    -- Create MVar to hold shared state: (sharer socket, list of viewer sockets)
    -- Initially, no sharer (Nothing) and no viewers ([])
    serverState <- newMVar (Nothing :: Maybe Socket, [] :: [Socket])

    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Accepted connection from " ++ show peer
        -- Pass the MVar and the new connection to the handler thread
        forkIO (handleClient serverState conn)

-- Modified handleClient to accept the server state MVar
handleClient :: ServerState -> Socket -> IO ()
handleClient serverState conn =
    -- Ensure the socket is closed and state is cleaned up when the thread exits
    finally (do
        -- Check if this is the sharer or a viewer
        isSharer <- modifyMVar serverState $ \(mSharer, viewers) ->
            case mSharer of
                Nothing -> do
                    -- No sharer yet, this connection becomes the sharer
                    putStrLn $ "Connection " ++ show conn ++ " assigned as Sharer."
                    return ((Just conn, viewers), True) -- Return new state and flag
                Just sharerConn -> do
                    -- Sharer already exists, this connection is a viewer
                    putStrLn $ "Connection " ++ show conn ++ " assigned as Viewer.\n"
                    return ((Just sharerConn, conn : viewers), False) -- Return new state and flag

        if isSharer
            then handleSharer serverState conn
            else handleViewer serverState conn

    ) (do
        -- This block runs when the thread exits (due to disconnect or error)
        putStrLn $ "Cleaning up connection: " ++ show conn
        modifyMVar_ serverState $ \(mSharer, viewers) ->
            case mSharer of
                Just s | s == conn -> do
                    putStrLn "Sharer disconnected."
                    return (Nothing, viewers) -- Remove sharer
                _ -> do
                    -- It must be a viewer connection
                    putStrLn $ "Viewer " ++ show conn ++ " disconnected."
                    return (mSharer, delete conn viewers) -- Remove from viewers
        close conn -- Close the socket
        putStrLn $ "Socket " ++ show conn ++ " closed."
    )

-- Handles the Sharer connection
-- Reads output from the sharer\'s PTY and forwards it to all connected viewers.
handleSharer :: ServerState -> Socket -> IO ()
handleSharer serverConnState sharerConn = forever $ do
    -- Read output from the sharer\'s PTY
    msg <- recv sharerConn 1024 -- Receive up to 1024 bytes
    if B.null msg
        then do
            -- Sharer disconnected (recv returned empty ByteString)
            putStrLn "Sharer recv loop detected disconnect."
            return () -- Exit the forever loop
        else do
            -- Received data from sharer (PTY output)
            -- Forward the data to all connected viewers
            (mSharer, viewers) <- readMVar serverConnState -- Read state atomically

            -- Send to each viewer, handling potential exceptions (e.g., viewer disconnects concurrently)
            mapM_ (\viewerConn -> handle (\(e :: IOException) -> putStrLn $ "Error sending to viewer " ++ show viewerConn ++ ": " ++ show e) $ sendAll viewerConn msg) viewers

-- Handles a Viewer connection
-- Reads input from the viewer and forwards it to the sharer\'s PTY.
handleViewer :: ServerState -> Socket -> IO ()
handleViewer serverConnState viewerConn = forever $ do
    msg <- recv viewerConn 1024
    if B.null msg
        then do
            -- Viewer disconnected
            putStrLn $ "Viewer " ++ show viewerConn ++ " recv loop detected disconnect."
            return () -- Exit the forever loop
    else do
        -- Received data from viewer (this is viewer\'s input for the PTY)
        -- putStrLn $ "Received " ++ show (B.length msg) ++ " bytes from viewer " ++ show viewerConn ++ ". Attempting to forward to sharer." -- Too verbose for server output

        -- Read the current state to get the sharer socket
        (mSharer, _) <- readMVar serverConnState

        case mSharer of
            Just sharerConn -> do
                -- Sharer exists, send the viewer\'s input to the sharer
                -- Handle potential exceptions if sharer disconnects during send
                handle (\(e :: IOException) -> putStrLn $ "Error sending viewer input to sharer " ++ show sharerConn ++ ": " ++ show e) $ do
                    sendAll sharerConn msg
                    -- putStrLn "Forwarded viewer input to sharer." -- Too verbose
            Nothing -> do
                -- No sharer connected, cannot forward input
                putStrLn $ "Received viewer input from " ++ show viewerConn ++ ", but no sharer is connected to forward to."

    -- A small delay might be useful if the loop is spinning too fast when there\'s no input.
    -- However, recv is blocking, so it shouldn\'t be necessary just for detecting disconnects.


resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE],
            addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) Nothing (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    return sock