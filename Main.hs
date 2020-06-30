-- Main.hs, final code
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Concurrent        (forkFinally)
import qualified Control.Exception         as E
import           Control.Monad             (forever, unless, void)
import qualified Data.ByteString           as S
import qualified Data.ByteString.Char8     as B
import           Data.ByteString.Internal
import           Data.ByteString.UTF8      as BLU
import           Data.Text.Lazy.Encoding
import Data.Time.Clock
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import           Prelude                   as P
import           Text.RawString.QQ
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TLIO
import Text.Format

main :: IO ()
main = runTCPServer Nothing "8000" talk
	where
		talk s = do
			putStrLn "before recv"
			msg <- recv s 1024 -- 这里是阻塞等待的
			putStrLn "after recv"
			unless (S.null msg) $ do
				let body = "<html><head><title>Hello World</title></head><h1>Hello World</h1></html>"
				let len =show $ P.length  body
				let alls = [r|
				HTTP/1.1 200 OK
				Accept-Ranges: bytes
				Content-Length: 75
				Content-Type: text/html
				Etag: "q99ylbk"
				Last-Modified: Fri, 24 Apr 2020 04:17:35 GMT
				Server: Caddy
				Date: Tue, 30 Jun 2020 05:57:32 GMT
				
				<html><head><title>Hello World</title></head><h1>Hello World</h1></html> |] 
				let res = BLU.fromString  [r|
HTTP/1.1 200 OK
Accept-Ranges: bytes
Content-Length: 75
Content-Type: text/html
Etag: "q99ylbk"
Last-Modified: Fri, 24 Apr 2020 04:17:35 GMT
Server: Caddy
Date: Tue, 30 Jun 2020 05:57:32 GMT

<html><head><title>Hello World</title></head><h1>Hello World</h1></html> |] 
				putStrLn $ show $ getRange 100 res
				sendAll s res
				-- talk s


getRange:: Int -> S.ByteString -> S.ByteString
getRange end s = S.take end s

runTCPServer:: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
	addr <- resolve
	putStrLn $ show addr
	E.bracket (open addr) close loop
	where
		resolve = do
			let hints = defaultHints {
				addrFlags = [AI_PASSIVE]
				, addrSocketType = Stream
			}
			head <$> getAddrInfo (Just hints) mhost (Just port)
		open addr = do
			sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
			setSocketOption sock ReuseAddr 1
			withFdSocket sock $ setCloseOnExecIfNeeded
			bind sock $ addrAddress addr
			listen sock 1024
			return sock
		loop sock = forever $ do
			putStrLn "loop runnning"
			(conn, _peer) <- accept sock
			void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
