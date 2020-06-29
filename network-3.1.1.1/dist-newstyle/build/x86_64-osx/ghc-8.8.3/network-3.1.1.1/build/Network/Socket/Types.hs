{-# LINE 1 "Network/Socket/Types.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


#include "HsNetDef.h"

module Network.Socket.Types (
    -- * Socket type
      Socket
    , withFdSocket
    , unsafeFdSocket
    , touchSocket
    , socketToFd
    , fdSocket
    , mkSocket
    , invalidateSocket
    , close
    , close'
    , c_close
    -- * Types of socket
    , SocketType(..)
    , isSupportedSocketType
    , packSocketType
    , packSocketType'
    , packSocketTypeOrThrow
    , unpackSocketType
    , unpackSocketType'

    -- * Family
    , Family(..)
    , isSupportedFamily
    , packFamily
    , unpackFamily

    -- * Socket address typeclass
    , SocketAddress(..)
    , withSocketAddress
    , withNewSocketAddress

    -- * Socket address type
    , SockAddr(..)
    , isSupportedSockAddr
    , HostAddress
    , hostAddressToTuple
    , tupleToHostAddress
    , HostAddress6
    , hostAddress6ToTuple
    , tupleToHostAddress6
    , FlowInfo
    , ScopeID
    , peekSockAddr
    , pokeSockAddr
    , withSockAddr

    -- * Unsorted
    , ProtocolNumber
    , defaultProtocol
    , PortNumber
    , defaultPort

    -- * Low-level helpers
    , zeroMemory
    , htonl
    , ntohl
    ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef', mkWeakIORef)
import Foreign.C.Error (throwErrno)
import Foreign.Marshal.Alloc
import GHC.Conc (closeFdWith)
import System.Posix.Types (Fd)
import Control.DeepSeq (NFData (..))
import GHC.Exts (touch#)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import GHC.IO (IO (..))


{-# LINE 84 "Network/Socket/Types.hsc" #-}
import Foreign.Marshal.Array

{-# LINE 86 "Network/Socket/Types.hsc" #-}

import Network.Socket.Imports

-----------------------------------------------------------------------------

-- | Basic type for a socket.
data Socket = Socket !(IORef CInt) !CInt {- for Show -}

instance Show Socket where
    show (Socket _ ofd) = "<socket: " ++ show ofd ++ ">"

instance Eq Socket where
    Socket ref1 _ == Socket ref2 _ = ref1 == ref2

{-# DEPRECATED fdSocket "Use withFdSocket or unsafeFdSocket instead" #-}
-- | Currently, this is an alias of `unsafeFdSocket`.
fdSocket :: Socket -> IO CInt
fdSocket = unsafeFdSocket

-- | Getting a file descriptor from a socket.
--
--   If a 'Socket' is shared with multiple threads and
--   one uses 'unsafeFdSocket', unexpected issues may happen.
--   Consider the following scenario:
--
--   1) Thread A acquires a 'Fd' from 'Socket' by 'unsafeFdSocket'.
--
--   2) Thread B close the 'Socket'.
--
--   3) Thread C opens a new 'Socket'. Unfortunately it gets the same 'Fd'
--      number which thread A is holding.
--
--   In this case, it is safer for Thread A to clone 'Fd' by
--   'System.Posix.IO.dup'. But this would still suffer from
--   a race condition between 'unsafeFdSocket' and 'close'.
--
--   If you use this function, you need to guarantee that the 'Socket' does not
--   get garbage-collected until after you finish using the file descriptor.
--   'touchSocket' can be used for this purpose.
--
--   A safer option is to use 'withFdSocket' instead.
unsafeFdSocket :: Socket -> IO CInt
unsafeFdSocket (Socket ref _) = readIORef ref

-- | Ensure that the given 'Socket' stays alive (i.e. not garbage-collected)
--   at the given place in the sequence of IO actions. This function can be
--   used in conjunction with 'unsafeFdSocket' to guarantee that the file
--   descriptor is not prematurely freed.
--
-- > fd <- unsafeFdSocket sock
-- > -- using fd with blocking operations such as accept(2)
-- > touchSocket sock
touchSocket :: Socket -> IO ()
touchSocket (Socket ref _) = touch ref

touch :: IORef a -> IO ()
touch (IORef (STRef mutVar)) =
  -- Thanks to a GHC issue, this touch# may not be quite guaranteed
  -- to work. There's talk of replacing the touch# primop with one
  -- that works better with the optimizer. But this seems to be the
  -- "right" way to do it for now.
  IO $ \s -> (# touch# mutVar s, () #)

-- | Get a file descriptor from a 'Socket'. The socket will never
-- be closed automatically before @withFdSocket@ completes, but
-- it may still be closed by an explicit call to 'close' or `close'`,
-- either before or during the call.
--
-- The file descriptor must not be used after @withFdSocket@ returns, because
-- the 'Socket' may have been garbage-collected, invalidating the file
-- descriptor.
--
-- Since: 3.1.0.0
withFdSocket :: Socket -> (CInt -> IO r) -> IO r
withFdSocket (Socket ref _) f = do
  fd <- readIORef ref
  -- Should we throw an exception if the socket is already invalid?
  -- That will catch some mistakes but certainly not all.

  r <- f fd

  touch ref
  return r

-- | Socket is closed and a duplicated file descriptor is returned.
--   The duplicated descriptor is no longer subject to the possibility
--   of unexpectedly being closed if the socket is finalized. It is
--   now the caller's responsibility to ultimately close the
--   duplicated file descriptor.
socketToFd :: Socket -> IO CInt
socketToFd s = do

{-# LINE 187 "Network/Socket/Types.hsc" #-}
    fd <- unsafeFdSocket s
    -- FIXME: throw error no if -1
    fd2 <- c_dup fd
    close s
    return fd2

foreign import ccall unsafe "dup"
   c_dup :: CInt -> IO CInt

{-# LINE 196 "Network/Socket/Types.hsc" #-}

-- | Creating a socket from a file descriptor.
mkSocket :: CInt -> IO Socket
mkSocket fd = do
    ref <- newIORef fd
    let s = Socket ref fd
    void $ mkWeakIORef ref $ close s
    return s

invalidSocket :: CInt

{-# LINE 209 "Network/Socket/Types.hsc" #-}
invalidSocket = -1

{-# LINE 211 "Network/Socket/Types.hsc" #-}

invalidateSocket ::
      Socket
   -> (CInt -> IO a)
   -> (CInt -> IO a)
   -> IO a
invalidateSocket (Socket ref _) errorAction normalAction = do
    oldfd <- atomicModifyIORef' ref $ \cur -> (invalidSocket, cur)
    if oldfd == invalidSocket then errorAction oldfd else normalAction oldfd

-----------------------------------------------------------------------------

-- | Close the socket. This function does not throw exceptions even if
--   the underlying system call returns errors.
--
--   If multiple threads use the same socket and one uses 'unsafeFdSocket' and
--   the other use 'close', unexpected behavior may happen.
--   For more information, please refer to the documentation of 'unsafeFdSocket'.
close :: Socket -> IO ()
close s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    -- closeFd ignores the return value of c_close and
    -- does not throw exceptions
    closeFd :: Fd -> IO ()
    closeFd = void . c_close . fromIntegral

-- | Close the socket. This function throws exceptions if
--   the underlying system call returns errors.
close' :: Socket -> IO ()
close' s = invalidateSocket s (\_ -> return ()) $ \oldfd -> do
    -- closeFdWith avoids the deadlock of IO manager.
    closeFdWith closeFd (toFd oldfd)
  where
    toFd :: CInt -> Fd
    toFd = fromIntegral
    closeFd :: Fd -> IO ()
    closeFd fd = do
        ret <- c_close $ fromIntegral fd
        when (ret == -1) $ throwErrno "Network.Socket.close'"


{-# LINE 259 "Network/Socket/Types.hsc" #-}
foreign import ccall unsafe "close"
  c_close :: CInt -> IO CInt

{-# LINE 262 "Network/Socket/Types.hsc" #-}

-----------------------------------------------------------------------------

-- | Protocol number.
type ProtocolNumber = CInt

-- | This is the default protocol for a given service.
--
-- >>> defaultProtocol
-- 0
defaultProtocol :: ProtocolNumber
defaultProtocol = 0

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

-- | Socket Types.
--
-- The existence of a constructor does not necessarily imply that that
-- socket type is supported on your system: see 'isSupportedSocketType'.
data SocketType
        = NoSocketType -- ^ 0, used in getAddrInfo hints, for example
        | Stream -- ^ SOCK_STREAM
        | Datagram -- ^ SOCK_DGRAM
        | Raw -- ^ SOCK_RAW
        | RDM -- ^ SOCK_RDM
        | SeqPacket -- ^ SOCK_SEQPACKET
        deriving (Eq, Ord, Read, Show, Typeable)

-- | Does the SOCK_ constant corresponding to the given SocketType exist on
-- this system?
isSupportedSocketType :: SocketType -> Bool
isSupportedSocketType = isJust . packSocketType'

-- | Find the SOCK_ constant corresponding to the SocketType value.
packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType -> Just 0

{-# LINE 317 "Network/Socket/Types.hsc" #-}
    Just Stream -> Just 1
{-# LINE 318 "Network/Socket/Types.hsc" #-}

{-# LINE 319 "Network/Socket/Types.hsc" #-}

{-# LINE 320 "Network/Socket/Types.hsc" #-}
    Just Datagram -> Just 2
{-# LINE 321 "Network/Socket/Types.hsc" #-}

{-# LINE 322 "Network/Socket/Types.hsc" #-}

{-# LINE 323 "Network/Socket/Types.hsc" #-}
    Just Raw -> Just 3
{-# LINE 324 "Network/Socket/Types.hsc" #-}

{-# LINE 325 "Network/Socket/Types.hsc" #-}

{-# LINE 326 "Network/Socket/Types.hsc" #-}
    Just RDM -> Just 4
{-# LINE 327 "Network/Socket/Types.hsc" #-}

{-# LINE 328 "Network/Socket/Types.hsc" #-}

{-# LINE 329 "Network/Socket/Types.hsc" #-}
    Just SeqPacket -> Just 5
{-# LINE 330 "Network/Socket/Types.hsc" #-}

{-# LINE 331 "Network/Socket/Types.hsc" #-}
    _ -> Nothing

packSocketType :: SocketType -> CInt
packSocketType stype = fromMaybe (error errMsg) (packSocketType' stype)
  where
    errMsg = concat ["Network.Socket.packSocketType: ",
                     "socket type ", show stype, " unsupported on this system"]

-- | Try packSocketType' on the SocketType, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
packSocketTypeOrThrow :: String -> SocketType -> IO CInt
packSocketTypeOrThrow caller stype = maybe err return (packSocketType' stype)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show stype, " unsupported on this system"]


unpackSocketType:: CInt -> Maybe SocketType
unpackSocketType t = case t of
        0 -> Just NoSocketType

{-# LINE 352 "Network/Socket/Types.hsc" #-}
        (1) -> Just Stream
{-# LINE 353 "Network/Socket/Types.hsc" #-}

{-# LINE 354 "Network/Socket/Types.hsc" #-}

{-# LINE 355 "Network/Socket/Types.hsc" #-}
        (2) -> Just Datagram
{-# LINE 356 "Network/Socket/Types.hsc" #-}

{-# LINE 357 "Network/Socket/Types.hsc" #-}

{-# LINE 358 "Network/Socket/Types.hsc" #-}
        (3) -> Just Raw
{-# LINE 359 "Network/Socket/Types.hsc" #-}

{-# LINE 360 "Network/Socket/Types.hsc" #-}

{-# LINE 361 "Network/Socket/Types.hsc" #-}
        (4) -> Just RDM
{-# LINE 362 "Network/Socket/Types.hsc" #-}

{-# LINE 363 "Network/Socket/Types.hsc" #-}

{-# LINE 364 "Network/Socket/Types.hsc" #-}
        (5) -> Just SeqPacket
{-# LINE 365 "Network/Socket/Types.hsc" #-}

{-# LINE 366 "Network/Socket/Types.hsc" #-}
        _ -> Nothing

-- | Try unpackSocketType on the CInt, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
unpackSocketType' :: String -> CInt -> IO SocketType
unpackSocketType' caller ty = maybe err return (unpackSocketType ty)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show ty, " unsupported on this system"]

------------------------------------------------------------------------
-- Protocol Families.

-- | Address families.
--
-- A constructor being present here does not mean it is supported by the
-- operating system: see 'isSupportedFamily'.
data Family
    = AF_UNSPEC           -- ^ unspecified
    | AF_UNIX             -- ^ UNIX-domain
    | AF_INET             -- ^ Internet Protocol version 4
    | AF_INET6            -- ^ Internet Protocol version 6
    | AF_IMPLINK          -- ^ Arpanet imp addresses
    | AF_PUP              -- ^ pup protocols: e.g. BSP
    | AF_CHAOS            -- ^ mit CHAOS protocols
    | AF_NS               -- ^ XEROX NS protocols
    | AF_NBS              -- ^ nbs protocols
    | AF_ECMA             -- ^ european computer manufacturers
    | AF_DATAKIT          -- ^ datakit protocols
    | AF_CCITT            -- ^ CCITT protocols, X.25 etc
    | AF_SNA              -- ^ IBM SNA
    | AF_DECnet           -- ^ DECnet
    | AF_DLI              -- ^ Direct data link interface
    | AF_LAT              -- ^ LAT
    | AF_HYLINK           -- ^ NSC Hyperchannel
    | AF_APPLETALK        -- ^ Apple Talk
    | AF_ROUTE            -- ^ Internal Routing Protocol (aka AF_NETLINK)
    | AF_NETBIOS          -- ^ NetBios-style addresses
    | AF_NIT              -- ^ Network Interface Tap
    | AF_802              -- ^ IEEE 802.2, also ISO 8802
    | AF_ISO              -- ^ ISO protocols
    | AF_OSI              -- ^ umbrella of all families used by OSI
    | AF_NETMAN           -- ^ DNA Network Management
    | AF_X25              -- ^ CCITT X.25
    | AF_AX25             -- ^ AX25
    | AF_OSINET           -- ^ AFI
    | AF_GOSSIP           -- ^ US Government OSI
    | AF_IPX              -- ^ Novell Internet Protocol
    | Pseudo_AF_XTP       -- ^ eXpress Transfer Protocol (no AF)
    | AF_CTF              -- ^ Common Trace Facility
    | AF_WAN              -- ^ Wide Area Network protocols
    | AF_SDL              -- ^ SGI Data Link for DLPI
    | AF_NETWARE          -- ^ Netware
    | AF_NDD              -- ^ NDD
    | AF_INTF             -- ^ Debugging use only
    | AF_COIP             -- ^ connection-oriented IP, aka ST II
    | AF_CNT              -- ^ Computer Network Technology
    | Pseudo_AF_RTIP      -- ^ Help Identify RTIP packets
    | Pseudo_AF_PIP       -- ^ Help Identify PIP packets
    | AF_SIP              -- ^ Simple Internet Protocol
    | AF_ISDN             -- ^ Integrated Services Digital Network
    | Pseudo_AF_KEY       -- ^ Internal key-management function
    | AF_NATM             -- ^ native ATM access
    | AF_ARP              -- ^ ARP (RFC 826)
    | Pseudo_AF_HDRCMPLT  -- ^ Used by BPF to not rewrite hdrs in iface output
    | AF_ENCAP            -- ^ ENCAP
    | AF_LINK             -- ^ Link layer interface
    | AF_RAW              -- ^ Link layer interface
    | AF_RIF              -- ^ raw interface
    | AF_NETROM           -- ^ Amateur radio NetROM
    | AF_BRIDGE           -- ^ multiprotocol bridge
    | AF_ATMPVC           -- ^ ATM PVCs
    | AF_ROSE             -- ^ Amateur Radio X.25 PLP
    | AF_NETBEUI          -- ^ Netbeui 802.2LLC
    | AF_SECURITY         -- ^ Security callback pseudo AF
    | AF_PACKET           -- ^ Packet family
    | AF_ASH              -- ^ Ash
    | AF_ECONET           -- ^ Acorn Econet
    | AF_ATMSVC           -- ^ ATM SVCs
    | AF_IRDA             -- ^ IRDA sockets
    | AF_PPPOX            -- ^ PPPoX sockets
    | AF_WANPIPE          -- ^ Wanpipe API sockets
    | AF_BLUETOOTH        -- ^ bluetooth sockets
    | AF_CAN              -- ^ Controller Area Network
      deriving (Eq, Ord, Read, Show)

-- | Converting 'Family' to 'CInt'.
packFamily :: Family -> CInt
packFamily f = case packFamily' f of
    Just fam -> fam
    Nothing -> error $
               "Network.Socket.packFamily: unsupported address family: " ++
               show f

-- | Does the AF_ constant corresponding to the given family exist on this
-- system?
isSupportedFamily :: Family -> Bool
isSupportedFamily = isJust . packFamily'

packFamily' :: Family -> Maybe CInt
packFamily' f = case Just f of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just AF_UNSPEC -> Just 0
{-# LINE 470 "Network/Socket/Types.hsc" #-}

{-# LINE 471 "Network/Socket/Types.hsc" #-}
    Just AF_UNIX -> Just 1
{-# LINE 472 "Network/Socket/Types.hsc" #-}

{-# LINE 473 "Network/Socket/Types.hsc" #-}

{-# LINE 474 "Network/Socket/Types.hsc" #-}
    Just AF_INET -> Just 2
{-# LINE 475 "Network/Socket/Types.hsc" #-}

{-# LINE 476 "Network/Socket/Types.hsc" #-}

{-# LINE 477 "Network/Socket/Types.hsc" #-}
    Just AF_INET6 -> Just 30
{-# LINE 478 "Network/Socket/Types.hsc" #-}

{-# LINE 479 "Network/Socket/Types.hsc" #-}

{-# LINE 480 "Network/Socket/Types.hsc" #-}
    Just AF_IMPLINK -> Just 3
{-# LINE 481 "Network/Socket/Types.hsc" #-}

{-# LINE 482 "Network/Socket/Types.hsc" #-}

{-# LINE 483 "Network/Socket/Types.hsc" #-}
    Just AF_PUP -> Just 4
{-# LINE 484 "Network/Socket/Types.hsc" #-}

{-# LINE 485 "Network/Socket/Types.hsc" #-}

{-# LINE 486 "Network/Socket/Types.hsc" #-}
    Just AF_CHAOS -> Just 5
{-# LINE 487 "Network/Socket/Types.hsc" #-}

{-# LINE 488 "Network/Socket/Types.hsc" #-}

{-# LINE 489 "Network/Socket/Types.hsc" #-}
    Just AF_NS -> Just 6
{-# LINE 490 "Network/Socket/Types.hsc" #-}

{-# LINE 491 "Network/Socket/Types.hsc" #-}

{-# LINE 494 "Network/Socket/Types.hsc" #-}

{-# LINE 495 "Network/Socket/Types.hsc" #-}
    Just AF_ECMA -> Just 8
{-# LINE 496 "Network/Socket/Types.hsc" #-}

{-# LINE 497 "Network/Socket/Types.hsc" #-}

{-# LINE 498 "Network/Socket/Types.hsc" #-}
    Just AF_DATAKIT -> Just 9
{-# LINE 499 "Network/Socket/Types.hsc" #-}

{-# LINE 500 "Network/Socket/Types.hsc" #-}

{-# LINE 501 "Network/Socket/Types.hsc" #-}
    Just AF_CCITT -> Just 10
{-# LINE 502 "Network/Socket/Types.hsc" #-}

{-# LINE 503 "Network/Socket/Types.hsc" #-}

{-# LINE 504 "Network/Socket/Types.hsc" #-}
    Just AF_SNA -> Just 11
{-# LINE 505 "Network/Socket/Types.hsc" #-}

{-# LINE 506 "Network/Socket/Types.hsc" #-}

{-# LINE 507 "Network/Socket/Types.hsc" #-}
    Just AF_DECnet -> Just 12
{-# LINE 508 "Network/Socket/Types.hsc" #-}

{-# LINE 509 "Network/Socket/Types.hsc" #-}

{-# LINE 510 "Network/Socket/Types.hsc" #-}
    Just AF_DLI -> Just 13
{-# LINE 511 "Network/Socket/Types.hsc" #-}

{-# LINE 512 "Network/Socket/Types.hsc" #-}

{-# LINE 513 "Network/Socket/Types.hsc" #-}
    Just AF_LAT -> Just 14
{-# LINE 514 "Network/Socket/Types.hsc" #-}

{-# LINE 515 "Network/Socket/Types.hsc" #-}

{-# LINE 516 "Network/Socket/Types.hsc" #-}
    Just AF_HYLINK -> Just 15
{-# LINE 517 "Network/Socket/Types.hsc" #-}

{-# LINE 518 "Network/Socket/Types.hsc" #-}

{-# LINE 519 "Network/Socket/Types.hsc" #-}
    Just AF_APPLETALK -> Just 16
{-# LINE 520 "Network/Socket/Types.hsc" #-}

{-# LINE 521 "Network/Socket/Types.hsc" #-}

{-# LINE 522 "Network/Socket/Types.hsc" #-}
    Just AF_ROUTE -> Just 17
{-# LINE 523 "Network/Socket/Types.hsc" #-}

{-# LINE 524 "Network/Socket/Types.hsc" #-}

{-# LINE 525 "Network/Socket/Types.hsc" #-}
    Just AF_NETBIOS -> Just 33
{-# LINE 526 "Network/Socket/Types.hsc" #-}

{-# LINE 527 "Network/Socket/Types.hsc" #-}

{-# LINE 530 "Network/Socket/Types.hsc" #-}

{-# LINE 533 "Network/Socket/Types.hsc" #-}

{-# LINE 534 "Network/Socket/Types.hsc" #-}
    Just AF_ISO -> Just 7
{-# LINE 535 "Network/Socket/Types.hsc" #-}

{-# LINE 536 "Network/Socket/Types.hsc" #-}

{-# LINE 537 "Network/Socket/Types.hsc" #-}
    Just AF_OSI -> Just 7
{-# LINE 538 "Network/Socket/Types.hsc" #-}

{-# LINE 539 "Network/Socket/Types.hsc" #-}

{-# LINE 542 "Network/Socket/Types.hsc" #-}

{-# LINE 545 "Network/Socket/Types.hsc" #-}

{-# LINE 548 "Network/Socket/Types.hsc" #-}

{-# LINE 551 "Network/Socket/Types.hsc" #-}

{-# LINE 554 "Network/Socket/Types.hsc" #-}

{-# LINE 555 "Network/Socket/Types.hsc" #-}
    Just AF_IPX -> Just 23
{-# LINE 556 "Network/Socket/Types.hsc" #-}

{-# LINE 557 "Network/Socket/Types.hsc" #-}

{-# LINE 560 "Network/Socket/Types.hsc" #-}

{-# LINE 563 "Network/Socket/Types.hsc" #-}

{-# LINE 566 "Network/Socket/Types.hsc" #-}

{-# LINE 569 "Network/Socket/Types.hsc" #-}

{-# LINE 572 "Network/Socket/Types.hsc" #-}

{-# LINE 575 "Network/Socket/Types.hsc" #-}

{-# LINE 578 "Network/Socket/Types.hsc" #-}

{-# LINE 579 "Network/Socket/Types.hsc" #-}
    Just AF_COIP -> Just 20
{-# LINE 580 "Network/Socket/Types.hsc" #-}

{-# LINE 581 "Network/Socket/Types.hsc" #-}

{-# LINE 582 "Network/Socket/Types.hsc" #-}
    Just AF_CNT -> Just 21
{-# LINE 583 "Network/Socket/Types.hsc" #-}

{-# LINE 584 "Network/Socket/Types.hsc" #-}

{-# LINE 587 "Network/Socket/Types.hsc" #-}

{-# LINE 590 "Network/Socket/Types.hsc" #-}

{-# LINE 591 "Network/Socket/Types.hsc" #-}
    Just AF_SIP -> Just 24
{-# LINE 592 "Network/Socket/Types.hsc" #-}

{-# LINE 593 "Network/Socket/Types.hsc" #-}

{-# LINE 594 "Network/Socket/Types.hsc" #-}
    Just AF_ISDN -> Just 28
{-# LINE 595 "Network/Socket/Types.hsc" #-}

{-# LINE 596 "Network/Socket/Types.hsc" #-}

{-# LINE 599 "Network/Socket/Types.hsc" #-}

{-# LINE 600 "Network/Socket/Types.hsc" #-}
    Just AF_NATM -> Just 31
{-# LINE 601 "Network/Socket/Types.hsc" #-}

{-# LINE 602 "Network/Socket/Types.hsc" #-}

{-# LINE 605 "Network/Socket/Types.hsc" #-}

{-# LINE 608 "Network/Socket/Types.hsc" #-}

{-# LINE 611 "Network/Socket/Types.hsc" #-}

{-# LINE 612 "Network/Socket/Types.hsc" #-}
    Just AF_LINK -> Just 18
{-# LINE 613 "Network/Socket/Types.hsc" #-}

{-# LINE 614 "Network/Socket/Types.hsc" #-}

{-# LINE 617 "Network/Socket/Types.hsc" #-}

{-# LINE 620 "Network/Socket/Types.hsc" #-}

{-# LINE 623 "Network/Socket/Types.hsc" #-}

{-# LINE 626 "Network/Socket/Types.hsc" #-}

{-# LINE 629 "Network/Socket/Types.hsc" #-}

{-# LINE 632 "Network/Socket/Types.hsc" #-}

{-# LINE 635 "Network/Socket/Types.hsc" #-}

{-# LINE 638 "Network/Socket/Types.hsc" #-}

{-# LINE 641 "Network/Socket/Types.hsc" #-}

{-# LINE 644 "Network/Socket/Types.hsc" #-}

{-# LINE 647 "Network/Socket/Types.hsc" #-}

{-# LINE 650 "Network/Socket/Types.hsc" #-}

{-# LINE 653 "Network/Socket/Types.hsc" #-}

{-# LINE 656 "Network/Socket/Types.hsc" #-}

{-# LINE 659 "Network/Socket/Types.hsc" #-}

{-# LINE 662 "Network/Socket/Types.hsc" #-}

{-# LINE 665 "Network/Socket/Types.hsc" #-}
    _ -> Nothing

--------- ----------

-- | Converting 'CInt' to 'Family'.
unpackFamily :: CInt -> Family
unpackFamily f = case f of
        (0) -> AF_UNSPEC
{-# LINE 673 "Network/Socket/Types.hsc" #-}

{-# LINE 674 "Network/Socket/Types.hsc" #-}
        (1) -> AF_UNIX
{-# LINE 675 "Network/Socket/Types.hsc" #-}

{-# LINE 676 "Network/Socket/Types.hsc" #-}

{-# LINE 677 "Network/Socket/Types.hsc" #-}
        (2) -> AF_INET
{-# LINE 678 "Network/Socket/Types.hsc" #-}

{-# LINE 679 "Network/Socket/Types.hsc" #-}

{-# LINE 680 "Network/Socket/Types.hsc" #-}
        (30) -> AF_INET6
{-# LINE 681 "Network/Socket/Types.hsc" #-}

{-# LINE 682 "Network/Socket/Types.hsc" #-}

{-# LINE 683 "Network/Socket/Types.hsc" #-}
        (3) -> AF_IMPLINK
{-# LINE 684 "Network/Socket/Types.hsc" #-}

{-# LINE 685 "Network/Socket/Types.hsc" #-}

{-# LINE 686 "Network/Socket/Types.hsc" #-}
        (4) -> AF_PUP
{-# LINE 687 "Network/Socket/Types.hsc" #-}

{-# LINE 688 "Network/Socket/Types.hsc" #-}

{-# LINE 689 "Network/Socket/Types.hsc" #-}
        (5) -> AF_CHAOS
{-# LINE 690 "Network/Socket/Types.hsc" #-}

{-# LINE 691 "Network/Socket/Types.hsc" #-}

{-# LINE 692 "Network/Socket/Types.hsc" #-}
        (6) -> AF_NS
{-# LINE 693 "Network/Socket/Types.hsc" #-}

{-# LINE 694 "Network/Socket/Types.hsc" #-}

{-# LINE 697 "Network/Socket/Types.hsc" #-}

{-# LINE 698 "Network/Socket/Types.hsc" #-}
        (8) -> AF_ECMA
{-# LINE 699 "Network/Socket/Types.hsc" #-}

{-# LINE 700 "Network/Socket/Types.hsc" #-}

{-# LINE 701 "Network/Socket/Types.hsc" #-}
        (9) -> AF_DATAKIT
{-# LINE 702 "Network/Socket/Types.hsc" #-}

{-# LINE 703 "Network/Socket/Types.hsc" #-}

{-# LINE 704 "Network/Socket/Types.hsc" #-}
        (10) -> AF_CCITT
{-# LINE 705 "Network/Socket/Types.hsc" #-}

{-# LINE 706 "Network/Socket/Types.hsc" #-}

{-# LINE 707 "Network/Socket/Types.hsc" #-}
        (11) -> AF_SNA
{-# LINE 708 "Network/Socket/Types.hsc" #-}

{-# LINE 709 "Network/Socket/Types.hsc" #-}

{-# LINE 710 "Network/Socket/Types.hsc" #-}
        (12) -> AF_DECnet
{-# LINE 711 "Network/Socket/Types.hsc" #-}

{-# LINE 712 "Network/Socket/Types.hsc" #-}

{-# LINE 713 "Network/Socket/Types.hsc" #-}
        (13) -> AF_DLI
{-# LINE 714 "Network/Socket/Types.hsc" #-}

{-# LINE 715 "Network/Socket/Types.hsc" #-}

{-# LINE 716 "Network/Socket/Types.hsc" #-}
        (14) -> AF_LAT
{-# LINE 717 "Network/Socket/Types.hsc" #-}

{-# LINE 718 "Network/Socket/Types.hsc" #-}

{-# LINE 719 "Network/Socket/Types.hsc" #-}
        (15) -> AF_HYLINK
{-# LINE 720 "Network/Socket/Types.hsc" #-}

{-# LINE 721 "Network/Socket/Types.hsc" #-}

{-# LINE 722 "Network/Socket/Types.hsc" #-}
        (16) -> AF_APPLETALK
{-# LINE 723 "Network/Socket/Types.hsc" #-}

{-# LINE 724 "Network/Socket/Types.hsc" #-}

{-# LINE 725 "Network/Socket/Types.hsc" #-}
        (17) -> AF_ROUTE
{-# LINE 726 "Network/Socket/Types.hsc" #-}

{-# LINE 727 "Network/Socket/Types.hsc" #-}

{-# LINE 728 "Network/Socket/Types.hsc" #-}
        (33) -> AF_NETBIOS
{-# LINE 729 "Network/Socket/Types.hsc" #-}

{-# LINE 730 "Network/Socket/Types.hsc" #-}

{-# LINE 733 "Network/Socket/Types.hsc" #-}

{-# LINE 736 "Network/Socket/Types.hsc" #-}

{-# LINE 737 "Network/Socket/Types.hsc" #-}
        (7) -> AF_ISO
{-# LINE 738 "Network/Socket/Types.hsc" #-}

{-# LINE 739 "Network/Socket/Types.hsc" #-}

{-# LINE 740 "Network/Socket/Types.hsc" #-}

{-# LINE 743 "Network/Socket/Types.hsc" #-}

{-# LINE 744 "Network/Socket/Types.hsc" #-}

{-# LINE 747 "Network/Socket/Types.hsc" #-}

{-# LINE 750 "Network/Socket/Types.hsc" #-}

{-# LINE 753 "Network/Socket/Types.hsc" #-}

{-# LINE 756 "Network/Socket/Types.hsc" #-}

{-# LINE 759 "Network/Socket/Types.hsc" #-}

{-# LINE 760 "Network/Socket/Types.hsc" #-}
        (23) -> AF_IPX
{-# LINE 761 "Network/Socket/Types.hsc" #-}

{-# LINE 762 "Network/Socket/Types.hsc" #-}

{-# LINE 765 "Network/Socket/Types.hsc" #-}

{-# LINE 768 "Network/Socket/Types.hsc" #-}

{-# LINE 771 "Network/Socket/Types.hsc" #-}

{-# LINE 774 "Network/Socket/Types.hsc" #-}

{-# LINE 777 "Network/Socket/Types.hsc" #-}

{-# LINE 780 "Network/Socket/Types.hsc" #-}

{-# LINE 783 "Network/Socket/Types.hsc" #-}

{-# LINE 784 "Network/Socket/Types.hsc" #-}
        (20) -> AF_COIP
{-# LINE 785 "Network/Socket/Types.hsc" #-}

{-# LINE 786 "Network/Socket/Types.hsc" #-}

{-# LINE 787 "Network/Socket/Types.hsc" #-}
        (21) -> AF_CNT
{-# LINE 788 "Network/Socket/Types.hsc" #-}

{-# LINE 789 "Network/Socket/Types.hsc" #-}

{-# LINE 792 "Network/Socket/Types.hsc" #-}

{-# LINE 795 "Network/Socket/Types.hsc" #-}

{-# LINE 796 "Network/Socket/Types.hsc" #-}
        (24) -> AF_SIP
{-# LINE 797 "Network/Socket/Types.hsc" #-}

{-# LINE 798 "Network/Socket/Types.hsc" #-}

{-# LINE 799 "Network/Socket/Types.hsc" #-}
        (28) -> AF_ISDN
{-# LINE 800 "Network/Socket/Types.hsc" #-}

{-# LINE 801 "Network/Socket/Types.hsc" #-}

{-# LINE 804 "Network/Socket/Types.hsc" #-}

{-# LINE 805 "Network/Socket/Types.hsc" #-}
        (31) -> AF_NATM
{-# LINE 806 "Network/Socket/Types.hsc" #-}

{-# LINE 807 "Network/Socket/Types.hsc" #-}

{-# LINE 810 "Network/Socket/Types.hsc" #-}

{-# LINE 813 "Network/Socket/Types.hsc" #-}

{-# LINE 816 "Network/Socket/Types.hsc" #-}

{-# LINE 817 "Network/Socket/Types.hsc" #-}
        (18) -> AF_LINK
{-# LINE 818 "Network/Socket/Types.hsc" #-}

{-# LINE 819 "Network/Socket/Types.hsc" #-}

{-# LINE 822 "Network/Socket/Types.hsc" #-}

{-# LINE 825 "Network/Socket/Types.hsc" #-}

{-# LINE 828 "Network/Socket/Types.hsc" #-}

{-# LINE 831 "Network/Socket/Types.hsc" #-}

{-# LINE 834 "Network/Socket/Types.hsc" #-}

{-# LINE 837 "Network/Socket/Types.hsc" #-}

{-# LINE 840 "Network/Socket/Types.hsc" #-}

{-# LINE 843 "Network/Socket/Types.hsc" #-}

{-# LINE 846 "Network/Socket/Types.hsc" #-}

{-# LINE 849 "Network/Socket/Types.hsc" #-}

{-# LINE 852 "Network/Socket/Types.hsc" #-}

{-# LINE 855 "Network/Socket/Types.hsc" #-}

{-# LINE 858 "Network/Socket/Types.hsc" #-}

{-# LINE 861 "Network/Socket/Types.hsc" #-}

{-# LINE 864 "Network/Socket/Types.hsc" #-}

{-# LINE 867 "Network/Socket/Types.hsc" #-}

{-# LINE 870 "Network/Socket/Types.hsc" #-}
        unknown -> error $
          "Network.Socket.Types.unpackFamily: unknown address family: " ++
          show unknown

------------------------------------------------------------------------
-- Port Numbers

-- | Port number.
--   Use the @Num@ instance (i.e. use a literal) to create a
--   @PortNumber@ value.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
-- >>> show (12345 :: PortNumber)
-- "12345"
-- >>> 50000 < (51000 :: PortNumber)
-- True
-- >>> 50000 < (52000 :: PortNumber)
-- True
-- >>> 50000 + (10000 :: PortNumber)
-- 60000
newtype PortNumber = PortNum Word16 deriving (Eq, Ord, Typeable, Num, Enum, Real, Integral)

-- Print "n" instead of "PortNum n".
instance Show PortNumber where
  showsPrec p (PortNum pn) = showsPrec p (fromIntegral pn :: Int)

-- Read "n" instead of "PortNum n".
instance Read PortNumber where
  readsPrec n = map (\(x,y) -> (fromIntegral (x :: Int), y)) . readsPrec n

foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
-- | Converts the from host byte order to network byte order.
foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
-- | Converts the from network byte order to host byte order.
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
{-# DEPRECATED htonl "Use getAddrInfo instead" #-}
{-# DEPRECATED ntohl "Use getAddrInfo instead" #-}

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) (htons po)
   peek p = PortNum . ntohs <$> peek (castPtr p)

-- | Default port number.
--
-- >>> defaultPort
-- 0
defaultPort :: PortNumber
defaultPort = 0

------------------------------------------------------------------------

-- | The core typeclass to unify socket addresses.
class SocketAddress sa where
    sizeOfSocketAddress :: sa -> Int
    peekSocketAddress :: Ptr sa -> IO sa
    pokeSocketAddress  :: Ptr a -> sa -> IO ()

-- sizeof(struct sockaddr_storage) which has enough space to contain
-- sockaddr_in, sockaddr_in6 and sockaddr_un.
sockaddrStorageLen :: Int
sockaddrStorageLen = 128

withSocketAddress :: SocketAddress sa => sa -> (Ptr sa -> Int -> IO a) -> IO a
withSocketAddress addr f = do
    let sz = sizeOfSocketAddress addr
    allocaBytes sz $ \p -> pokeSocketAddress p addr >> f (castPtr p) sz

withNewSocketAddress :: SocketAddress sa => (Ptr sa -> Int -> IO a) -> IO a
withNewSocketAddress f = allocaBytes sockaddrStorageLen $ \ptr -> do
    zeroMemory ptr $ fromIntegral sockaddrStorageLen
    f ptr sockaddrStorageLen

------------------------------------------------------------------------
-- Socket addresses

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for UNIX-domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only UNIX-domain sockets and the Internet
-- families are supported.

-- | Flow information.
type FlowInfo = Word32
-- | Scope identifier.
type ScopeID = Word32

-- | Socket addresses.
--  The existence of a constructor does not necessarily imply that
--  that socket address type is supported on your system: see
-- 'isSupportedSockAddr'.
data SockAddr
  = SockAddrInet
        !PortNumber      -- sin_port
        !HostAddress     -- sin_addr  (ditto)
  | SockAddrInet6
        !PortNumber      -- sin6_port
        !FlowInfo        -- sin6_flowinfo (ditto)
        !HostAddress6    -- sin6_addr (ditto)
        !ScopeID         -- sin6_scope_id (ditto)
  -- | The path must have fewer than 104 characters. All of these characters must have code points less than 256.
  | SockAddrUnix
        String           -- sun_path
  deriving (Eq, Ord, Typeable)

instance NFData SockAddr where
  rnf (SockAddrInet _ _) = ()
  rnf (SockAddrInet6 _ _ _ _) = ()
  rnf (SockAddrUnix str) = rnf str

-- | Is the socket address type supported on this system?
isSupportedSockAddr :: SockAddr -> Bool
isSupportedSockAddr addr = case addr of
  SockAddrInet{}  -> True
  SockAddrInet6{} -> True

{-# LINE 1002 "Network/Socket/Types.hsc" #-}
  SockAddrUnix{}  -> True

{-# LINE 1006 "Network/Socket/Types.hsc" #-}

instance SocketAddress SockAddr where
    sizeOfSocketAddress = sizeOfSockAddr
    peekSocketAddress   = peekSockAddr
    pokeSocketAddress   = pokeSockAddr


{-# LINE 1015 "Network/Socket/Types.hsc" #-}
type CSaFamily = (Word8)
{-# LINE 1016 "Network/Socket/Types.hsc" #-}

{-# LINE 1019 "Network/Socket/Types.hsc" #-}

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> Int

{-# LINE 1025 "Network/Socket/Types.hsc" #-}

{-# LINE 1042 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrUnix{}  = 106
{-# LINE 1043 "Network/Socket/Types.hsc" #-}

{-# LINE 1044 "Network/Socket/Types.hsc" #-}

{-# LINE 1047 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrInet{}  = 16
{-# LINE 1048 "Network/Socket/Types.hsc" #-}
sizeOfSockAddr SockAddrInet6{} = 28
{-# LINE 1049 "Network/Socket/Types.hsc" #-}

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- 'SockAddr' and the length of that 'SockAddr'.
withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

-- We cannot bind sun_paths longer than than the space in the sockaddr_un
-- structure, and attempting to do so could overflow the allocated storage
-- space.  This constant holds the maximum allowable path length.
--

{-# LINE 1062 "Network/Socket/Types.hsc" #-}
unixPathMax :: Int
unixPathMax = 104
{-# LINE 1064 "Network/Socket/Types.hsc" #-}

{-# LINE 1065 "Network/Socket/Types.hsc" #-}

-- We can't write an instance of 'Storable' for 'SockAddr' because
-- @sockaddr@ is a sum type of variable size but
-- 'Foreign.Storable.sizeOf' is required to be constant.

-- Note that on Darwin, the sockaddr structure must be zeroed before
-- use.

-- | Write the given 'SockAddr' to the given memory location.
pokeSockAddr :: Ptr a -> SockAddr -> IO ()

{-# LINE 1076 "Network/Socket/Types.hsc" #-}
pokeSockAddr p sa@(SockAddrUnix path) = do
    when (length path > unixPathMax) $ error "pokeSockAddr: path is too long"
    zeroMemory p $ fromIntegral $ sizeOfSockAddr sa

{-# LINE 1080 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((106) :: Word8)
{-# LINE 1081 "Network/Socket/Types.hsc" #-}

{-# LINE 1082 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((1) :: CSaFamily)
{-# LINE 1083 "Network/Socket/Types.hsc" #-}
    let pathC = map castCharToCChar path
    -- the buffer is already filled with nulls.
    pokeArray (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p) pathC
{-# LINE 1086 "Network/Socket/Types.hsc" #-}

{-# LINE 1089 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet port addr) = do
    zeroMemory p (16)
{-# LINE 1091 "Network/Socket/Types.hsc" #-}

{-# LINE 1092 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((16) :: Word8)
{-# LINE 1093 "Network/Socket/Types.hsc" #-}

{-# LINE 1094 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((2) :: CSaFamily)
{-# LINE 1095 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 1096 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p addr
{-# LINE 1097 "Network/Socket/Types.hsc" #-}
pokeSockAddr p (SockAddrInet6 port flow addr scope) = do
    zeroMemory p (28)
{-# LINE 1099 "Network/Socket/Types.hsc" #-}

{-# LINE 1100 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((28) :: Word8)
{-# LINE 1101 "Network/Socket/Types.hsc" #-}

{-# LINE 1102 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 1)) p ((30) :: CSaFamily)
{-# LINE 1103 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 1104 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p flow
{-# LINE 1105 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (In6Addr addr)
{-# LINE 1106 "Network/Socket/Types.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p scope
{-# LINE 1107 "Network/Socket/Types.hsc" #-}

-- | Read a 'SockAddr' from the given memory location.
peekSockAddr :: Ptr SockAddr -> IO SockAddr
peekSockAddr p = do
  family <- ((\hsc_ptr -> peekByteOff hsc_ptr 1)) p
{-# LINE 1112 "Network/Socket/Types.hsc" #-}
  case family :: CSaFamily of

{-# LINE 1114 "Network/Socket/Types.hsc" #-}
    (1) -> do
{-# LINE 1115 "Network/Socket/Types.hsc" #-}
        str <- peekCAString (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p)
{-# LINE 1116 "Network/Socket/Types.hsc" #-}
        return (SockAddrUnix str)

{-# LINE 1118 "Network/Socket/Types.hsc" #-}
    (2) -> do
{-# LINE 1119 "Network/Socket/Types.hsc" #-}
        addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 1120 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 1121 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet port addr)
    (30) -> do
{-# LINE 1123 "Network/Socket/Types.hsc" #-}
        port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 1124 "Network/Socket/Types.hsc" #-}
        flow <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 1125 "Network/Socket/Types.hsc" #-}
        In6Addr addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 1126 "Network/Socket/Types.hsc" #-}
        scope <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 1127 "Network/Socket/Types.hsc" #-}
        return (SockAddrInet6 port flow addr scope)
    _ -> ioError $ userError $
      "Network.Socket.Types.peekSockAddr: address family '" ++
      show family ++ "' not supported."

------------------------------------------------------------------------

-- | The raw network byte order number is read using host byte order.
-- Therefore on little-endian architectures the byte order is swapped. For
-- example @127.0.0.1@ is represented as @0x0100007f@ on little-endian hosts
-- and as @0x7f000001@ on big-endian hosts.
--
-- For direct manipulation prefer 'hostAddressToTuple' and
-- 'tupleToHostAddress'.
type HostAddress = Word32

-- | Converts 'HostAddress' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(0x7f, 0, 0, 1)@
-- regardless of host endianness.
--
{- -- prop> tow == hostAddressToTuple (tupleToHostAddress tow) -}
hostAddressToTuple :: HostAddress -> (Word8, Word8, Word8, Word8)
hostAddressToTuple ha' =
    let ha = htonl ha'
        byte i = fromIntegral (ha `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'HostAddress'.
tupleToHostAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
tupleToHostAddress (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in ntohl $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'hostAddress6ToTuple' and
-- 'tupleToHostAddress6'.
type HostAddress6 = (Word32, Word32, Word32, Word32)

-- | Converts 'HostAddress6' to representation-independent IPv6 octuple.
--
{- -- prop> (w1,w2,w3,w4,w5,w6,w7,w8) == hostAddress6ToTuple (tupleToHostAddress6 (w1,w2,w3,w4,w5,w6,w7,w8)) -}
hostAddress6ToTuple :: HostAddress6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
hostAddress6ToTuple (w3, w2, w1, w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | Converts IPv6 octuple to 'HostAddress6'.
tupleToHostAddress6 :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> HostAddress6
tupleToHostAddress6 (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in (w7 `add` w6, w5 `add` w4, w3 `add` w2, w1 `add` w0)

-- The peek32 and poke32 functions work around the fact that the RFCs
-- don't require 32-bit-wide address fields to be present.  We can
-- only portably rely on an 8-bit field, s6_addr.

s6_addr_offset :: Int
s6_addr_offset = ((0))
{-# LINE 1191 "Network/Socket/Types.hsc" #-}

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

-- | Private newtype proxy for the Storable instance. To avoid orphan instances.
newtype In6Addr = In6Addr HostAddress6


{-# LINE 1219 "Network/Socket/Types.hsc" #-}

instance Storable In6Addr where
    sizeOf _    = 16
{-# LINE 1222 "Network/Socket/Types.hsc" #-}
    alignment _ = 4
{-# LINE 1223 "Network/Socket/Types.hsc" #-}

    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ In6Addr (a, b, c, d)

    poke p (In6Addr (a, b, c, d)) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d

------------------------------------------------------------------------
-- Helper functions

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Zero a structure.
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 (fromIntegral nbytes)
