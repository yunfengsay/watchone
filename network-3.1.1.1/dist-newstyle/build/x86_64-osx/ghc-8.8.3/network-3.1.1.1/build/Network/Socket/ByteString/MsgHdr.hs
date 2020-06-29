{-# LINE 1 "Network/Socket/ByteString/MsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX 'sendmsg' system call.
module Network.Socket.ByteString.MsgHdr
    ( MsgHdr(..)
    ) where




import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)
import Network.Socket.Types (SockAddr)

import Network.Socket.ByteString.IOVec (IOVec)

-- We don't use msg_control, msg_controllen, and msg_flags as these
-- don't exist on OpenSolaris.
data MsgHdr = MsgHdr
    { msgName    :: !(Ptr SockAddr)
    , msgNameLen :: !CUInt
    , msgIov     :: !(Ptr IOVec)
    , msgIovLen  :: !CSize
    }

instance Storable MsgHdr where
  sizeOf _    = (48)
{-# LINE 28 "Network/Socket/ByteString/MsgHdr.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))       p
{-# LINE 32 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    nameLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))    p
{-# LINE 33 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    iov        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))        p
{-# LINE 34 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    iovLen     <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     p
{-# LINE 35 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    return $ MsgHdr name nameLen iov iovLen

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (48)
{-# LINE 42 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))       p (msgName       mh)
{-# LINE 43 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p (msgNameLen    mh)
{-# LINE 44 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))        p (msgIov        mh)
{-# LINE 45 "Network/Socket/ByteString/MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24))     p (msgIovLen     mh)
{-# LINE 46 "Network/Socket/ByteString/MsgHdr.hsc" #-}
