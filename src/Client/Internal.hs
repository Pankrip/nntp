{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Client.Internal
	( ClientDescriptor (..)
	, OperationMode (..)
--	, repl
	, dispatchCommand
	, newClientDescriptor
	) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Word as W
import qualified Data.Char as C
-- import qualified Control.Concurrent as C
-- import qualified Control.Exception as E
import qualified Network.Socket as N
import qualified "unix-bytestring" System.Posix.IO.ByteString as I
import qualified System.Posix.Types as T
-- import qualified Foreign.C.Types as Ct
-- import qualified Data.Word as W
import qualified Commands as CMD
import Client.Descriptor

-- 
dispatchCommand :: L.ByteString -- 'ByteString' with NNTP command and its\' arguments
		-> ClientDescriptor -- ^ current client state
		-> IO ClientDescriptor -- ^ new client state
dispatchCommand buf cd = (
	-- return with QuitMode for everything (for testing purposes)
	-- undefined
	-- I.fdWrite (T.Fd $ N.fdSocket $ socket cd) ("goodbye!\r\n" :: S.ByteString) >>
	let
	args = (tokenise ("\r\n" :: S.ByteString) (L.toStrict buf))
	in
	if (S.map (toUpper) (head args)) == ("ARTICLE" :: S.ByteString) then
		CMD.article cd (tail args)
	else
		return cd { state = QuitMode }
	)

tokenise :: S.ByteString -> S.ByteString -> [S.ByteString]
tokenise delim s = (
	let
	(h, t) = S.breakSubstring delim s
	in
	h : if S.null t then [] else ( tokenise delim (S.drop (S.length delim) s) )
	)

toUpper :: W.Word8 -> W.Word8
toUpper c = (
	((toEnum :: Int -> W.Word8) . fromEnum) $
	(C.toUpper . ((toEnum :: Int -> Char) . fromEnum)) c
	)
