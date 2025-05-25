module Main where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as B
import Control.Monad (forever)
import System.IO (stdout, hFlush)

main :: IO ()
main = withSocketsDo $ do
    putStrLn "Viewer connecting to server on port 3000..."
    addr <- resolve "127.0.0.1" "3000"
    sock <- open addr
    putStrLn "Connected. Waiting for terminal data..."

    handleServerConnection sock

    putStrLn "Server connection closed. Viewer shutting down."
    close sock

handleServerConnection :: Socket -> IO ()
handleServerConnection sock = forever $ do
    msg <- recv sock 1024
    if B.null msg
        then do
            putStrLn "\nServer disconnected."
            return () -- Exit the forever loop
        else do
            B.putStr msg
            hFlush stdout

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock
