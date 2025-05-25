module Main where

import Network.Socket hiding (recv) -- Hide recv from Network.Socket to avoid ambiguity
import Network.Socket.ByteString (recv, sendAll) -- Import recv and sendAll for ByteString
import Control.Concurrent (forkIO, threadDelay) -- Added threadDelay (might remove later)
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
                    putStrLn $ "Connection " ++ show conn ++ " assigned as Viewer."
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
                    putStrLn "Viewer disconnected."
                    return (mSharer, delete conn viewers) -- Remove from viewers
        close conn -- Close the socket
    )

-- Handles the Sharer connection
handleSharer :: ServerState -> Socket -> IO ()
handleSharer serverConnState sharerConn = forever $ do
    -- Read output from the sharer's PTY
    msg <- recv sharerConn 1024 -- Receive up to 1024 bytes
    if B.null msg
        then do
            -- Sharer disconnected (recv returned empty ByteString)
            putStrLn "Sharer recv loop detected disconnect."
            return () -- Exit the forever loop
        else do
            -- Received data from sharer
            -- Print locally on the server (optional, mainly for debugging)
            -- B.putStr msg -- Removed local print to avoid server terminal clutter

            -- Forward the data to all connected viewers
            (mSharer, viewers) <- readMVar serverConnState -- Read state atomically
            -- Note: viewers list might change concurrently, but readMVar gives a consistent snapshot
            -- Need to handle potential exceptions during sendAll (e.g., viewer disconnects)
            mapM_ (\viewerConn -> handle (\e -> putStrLn $ "Error sending to viewer: " ++ show (e :: IOException)) $ sendAll viewerConn msg) viewers

-- Handles a Viewer connection
-- For now, viewers only receive data, they don't send PTY input.
-- We still need to read from the viewer socket to detect disconnection.
handleViewer :: ServerState -> Socket -> IO ()
handleViewer serverConnState viewerConn = forever $ do
    msg <- recv viewerConn 1024
    if B.null msg
        then do
            -- Viewer disconnected
            putStrLn "Viewer recv loop detected disconnect."
            return () -- Exit the forever loop
    else do
        -- Received data from viewer (unexpected input for now)
        -- In a real implementation, this would be PTY input to send back to the sharer
        -- B.putStrLn $ B.pack \"Server received unexpected data from viewer: \" <> msg -- Removed for cleaner output
        return () -- Added to make the do block non-empty

    -- A small delay might be useful in a real viewer handler if it has other tasks,
    -- but for just reading to detect disconnect, the B.null check is sufficient.
    -- threadDelay 10000 -- Example delay (10ms)


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