{-# LANGUAGE OverloadedStrings #-}

module Commands
	( article
	) where

import qualified Client.Descriptor as CD
import Network.Socket.ByteString
import qualified Data.ByteString as S

article :: CD.ClientDescriptor
        -> [S.ByteString]
        -> IO CD.ClientDescriptor
article cd args = sendAll (CD.socket cd) ("192 <article>\r\n" :: S.ByteString) >>
        return cd

