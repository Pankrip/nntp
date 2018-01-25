{-# LANGUAGE OverloadedStrings #-}

module Commands
	( 
	) where

import Client.Descriptor
import Netowork.Socket.ByteString

article :: ClientDescriptor
        -> [ByteString]
        -> IO ClientDescriptor
article cd args = sendAll $ socket cd $ ("xD" :: ByteString) >>
        return cd

