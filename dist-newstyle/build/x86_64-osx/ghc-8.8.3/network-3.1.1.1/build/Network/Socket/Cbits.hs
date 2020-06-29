{-# LINE 1 "Network/Socket/Cbits.hsc" #-}
module Network.Socket.Cbits where



import Network.Socket.Imports

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = 128
{-# LINE 12 "Network/Socket/Cbits.hsc" #-}


{-# LINE 17 "Network/Socket/Cbits.hsc" #-}
fGetFd :: CInt
fGetFd = 1
{-# LINE 19 "Network/Socket/Cbits.hsc" #-}
fGetFl :: CInt
fGetFl = 3
{-# LINE 21 "Network/Socket/Cbits.hsc" #-}
fdCloexec :: CInt
fdCloexec = 1
{-# LINE 23 "Network/Socket/Cbits.hsc" #-}
oNonBlock :: CInt
oNonBlock = 4
{-# LINE 25 "Network/Socket/Cbits.hsc" #-}

{-# LINE 31 "Network/Socket/Cbits.hsc" #-}

{-# LINE 32 "Network/Socket/Cbits.hsc" #-}
