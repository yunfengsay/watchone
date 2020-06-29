module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (unless, forever)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  _ <- forkIO $ runTCPEchoServer
  threadDelay 1000000 -- wait one second
  -- forever $ pure ()
  sendMessage "Hello, world!"
  threadDelay 1000000 -- wait one second

runTCPEchoServer :: IO ()
runTCPEchoServer = do
	addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
	let serveraddr = head addrinfos
	sock <- socket (addrFamily serveraddr) Stream defaultProtocol
	bind sock (addrAddress serveraddr)
  	listen sock 1
  	(conn, _) <- accept sock
  	print "TCP server is waiting for a message..."
  	msg <- recv conn 1024
  	unless (BS.null msg) $ do
  	  print ("TCP server received: " ++ C.unpack msg)
  	  print "TCP server is now sending a message to the client"
  	  sendAll conn msg
  	print "TCP server socket is closing now."
  	close conn
  	close sock

sendMessage :: String -> IO ()
sendMessage s = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack s
  msg <- recv sock 1024
  close sock
  -- delay thread to avoid client and server from printing at the same time
  threadDelay 1000000
  print ("TCP client received: " ++ C.unpack msg)
