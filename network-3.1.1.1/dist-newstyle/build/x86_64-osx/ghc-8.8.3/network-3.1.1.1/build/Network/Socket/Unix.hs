{-# LINE 1 "Network/Socket/Unix.hsc" #-}
{-# LANGUAGE CPP #-}


#include "HsNetDef.h"

module Network.Socket.Unix (
    isUnixDomainSocketAvailable
  , socketPair
  , sendFd
  , recvFd
  , getPeerCredential
  , getPeerCred
  , getPeerEid
  ) where

import Network.Socket.Imports
import Network.Socket.Types


{-# LINE 20 "Network/Socket/Unix.hsc" #-}
import System.IO.Error (catchIOError)

{-# LINE 22 "Network/Socket/Unix.hsc" #-}

{-# LINE 25 "Network/Socket/Unix.hsc" #-}

{-# LINE 26 "Network/Socket/Unix.hsc" #-}
import Foreign.Marshal.Alloc (alloca)

{-# LINE 28 "Network/Socket/Unix.hsc" #-}

{-# LINE 29 "Network/Socket/Unix.hsc" #-}
import Control.Monad (void)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Network.Socket.Fcntl
import Network.Socket.Internal

{-# LINE 38 "Network/Socket/Unix.hsc" #-}

{-# LINE 41 "Network/Socket/Unix.hsc" #-}

-- | Getting process ID, user ID and group ID for UNIX-domain sockets.
--
--   This is implemented with SO_PEERCRED on Linux and getpeereid()
--   on BSD variants. Unfortunately, on some BSD variants
--   getpeereid() returns unexpected results, rather than an error,
--   for AF_INET sockets. It is the user's responsibility to make sure
--   that the socket is a UNIX-domain socket.
--   Also, on some BSD variants, getpeereid() does not return credentials
--   for sockets created via 'socketPair', only separately created and then
--   explicitly connected UNIX-domain sockets work on such systems.
--
--   Since 2.7.0.0.
getPeerCredential :: Socket -> IO (Maybe CUInt, Maybe CUInt, Maybe CUInt)

{-# LINE 63 "Network/Socket/Unix.hsc" #-}
getPeerCredential sock =
    go `catchIOError` \_ -> return (Nothing,Nothing,Nothing)
  where
    go = do
        (uid, gid) <- getPeerEid sock
        return (Nothing, Just uid, Just gid)

{-# LINE 72 "Network/Socket/Unix.hsc" #-}

-- | Returns the processID, userID and groupID of the peer of
--   a UNIX-domain socket.
--
-- Only available on platforms that support SO_PEERCRED.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)

{-# LINE 90 "Network/Socket/Unix.hsc" #-}
getPeerCred _ = return (0, 0, 0)

{-# LINE 92 "Network/Socket/Unix.hsc" #-}
{-# Deprecated getPeerCred "Use getPeerCredential instead" #-}

-- | Returns the userID and groupID of the peer of
--   a UNIX-domain socket.
--
--  Only available on platforms that support getpeereid().
getPeerEid :: Socket -> IO (CUInt, CUInt)

{-# LINE 100 "Network/Socket/Unix.hsc" #-}
getPeerEid s = do
  alloca $ \ ptr_uid ->
    alloca $ \ ptr_gid -> do
      withFdSocket s $ \fd ->
        throwSocketErrorIfMinus1Retry_ "Network.Socket.getPeerEid" $
          c_getpeereid fd ptr_uid ptr_gid
      uid <- peek ptr_uid
      gid <- peek ptr_gid
      return (uid, gid)

foreign import CALLCONV unsafe "getpeereid"
  c_getpeereid :: CInt -> Ptr CUInt -> Ptr CUInt -> IO CInt

{-# LINE 115 "Network/Socket/Unix.hsc" #-}

{-# Deprecated getPeerEid "Use getPeerCredential instead" #-}

-- | Whether or not UNIX-domain sockets are available.
--
--   Since 2.7.0.0.
isUnixDomainSocketAvailable :: Bool

{-# LINE 123 "Network/Socket/Unix.hsc" #-}
isUnixDomainSocketAvailable = True

{-# LINE 127 "Network/Socket/Unix.hsc" #-}

-- | Send a file descriptor over a UNIX-domain socket.
--   Use this function in the case where 'isUnixDomainSocketAvailable' is
--  'True'.
sendFd :: Socket -> CInt -> IO ()

{-# LINE 133 "Network/Socket/Unix.hsc" #-}
sendFd s outfd = void $ do
  withFdSocket s $ \fd ->
    throwSocketErrorWaitWrite s "Network.Socket.sendFd" $ c_sendFd fd outfd
foreign import ccall SAFE_ON_WIN "sendFd" c_sendFd :: CInt -> CInt -> IO CInt

{-# LINE 140 "Network/Socket/Unix.hsc" #-}

-- | Receive a file descriptor over a UNIX-domain socket. Note that the resulting
--   file descriptor may have to be put into non-blocking mode in order to be
--   used safely. See 'setNonBlockIfNeeded'.
--   Use this function in the case where 'isUnixDomainSocketAvailable' is
--  'True'.
recvFd :: Socket -> IO CInt

{-# LINE 148 "Network/Socket/Unix.hsc" #-}
recvFd s = do
  withFdSocket s $ \fd ->
    throwSocketErrorWaitRead s "Network.Socket.recvFd" $ c_recvFd fd
foreign import ccall SAFE_ON_WIN "recvFd" c_recvFd :: CInt -> IO CInt

{-# LINE 155 "Network/Socket/Unix.hsc" #-}

-- | Build a pair of connected socket objects.
--   For portability, use this function in the case
--   where 'isUnixDomainSocketAvailable' is 'True'
--   and specify 'AF_UNIX' to the first argument.
socketPair :: Family              -- Family Name (usually AF_UNIX)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.

{-# LINE 165 "Network/Socket/Unix.hsc" #-}
socketPair family stype protocol =
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
      c_stype <- packSocketTypeOrThrow "socketPair" stype
      _rc <- throwSocketErrorIfMinus1Retry "Network.Socket.socketpair" $
                  c_socketpair (packFamily family) c_stype protocol fdArr
      [fd1,fd2] <- peekArray 2 fdArr
      setNonBlockIfNeeded fd1
      setNonBlockIfNeeded fd2
      s1 <- mkSocket fd1
      s2 <- mkSocket fd2
      return (s1, s2)

foreign import ccall unsafe "socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

{-# LINE 182 "Network/Socket/Unix.hsc" #-}
