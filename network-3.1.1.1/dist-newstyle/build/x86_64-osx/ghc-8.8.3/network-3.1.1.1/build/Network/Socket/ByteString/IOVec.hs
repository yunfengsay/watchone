{-# LINE 1 "Network/Socket/ByteString/IOVec.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX writev system call.
module Network.Socket.ByteString.IOVec
    ( IOVec(..)
    ) where

import Network.Socket.Imports




data IOVec = IOVec
    { iovBase :: !(Ptr CChar)
    , iovLen  :: !CSize
    }

instance Storable IOVec where
  sizeOf _    = (16)
{-# LINE 20 "Network/Socket/ByteString/IOVec.hsc" #-}
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 24 "Network/Socket/ByteString/IOVec.hsc" #-}
    len  <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))  p
{-# LINE 25 "Network/Socket/ByteString/IOVec.hsc" #-}
    return $ IOVec base len

  poke p iov = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (iovBase iov)
{-# LINE 29 "Network/Socket/ByteString/IOVec.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))  p (iovLen  iov)
{-# LINE 30 "Network/Socket/ByteString/IOVec.hsc" #-}
