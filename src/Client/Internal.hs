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
-- import qualified Control.Concurrent as C
-- import qualified Control.Exception as E
import qualified Network.Socket as N
import qualified "unix-bytestring" System.Posix.IO.ByteString as I
import qualified System.Posix.Types as T
-- import qualified Foreign.C.Types as Ct
-- import qualified Data.Word as W
--
-- import qualified Group as G
-- import qualified Article as A

-- | structure describing client state
data ClientDescriptor = ClientDescriptor
	{ state :: OperationMode -- ^ state of the state machine of client connection
	, socket :: N.Socket -- ^ socket used for communication with client
	, authinfo :: Maybe L.ByteString -- ^ 'Just' username of authenticated client, or 'Nothing'
	, lastCmd :: Maybe L.ByteString -- ^ last executed command (used to determine which command text body belongs to)
--	, group :: Maybe G.Group -- ^ current selected group
--	, article :: Maybe A.Article -- ^ current article pointer
	} deriving (Show, Eq)


-- | Returns 'ClientDescriptor' for fresh connection
newClientDescriptor :: N.Socket -> ClientDescriptor
newClientDescriptor sock = ClientDescriptor
	{ state = CommandMode
	, socket = sock
	, authinfo = Nothing
	, lastCmd = Nothing
--	, group = Nothing
--	, article = Nothing
	}

-- | Datatype indicating the state client is in
data OperationMode = CommandMode -- ^ indicates that client is sending regular commands
		   | PostMode -- ^ indicates that client is uploading a post (eg. after a POST or IHAVE command)
		   | QuitMode -- ^ indicates that server must terminate the connection
		   deriving (Eq, Show)


dispatchCommand :: L.ByteString -- 'ByteString' with NNTP command and its\' arguments
		-> ClientDescriptor -- ^ current client state
		-> IO ClientDescriptor -- ^ new client state
dispatchCommand buf cd = (
	-- return with QuitMode for everything (for testing purposes)
	-- undefined
	I.fdWrite (T.Fd $ N.fdSocket $ socket cd) ("goodbye!\r\n" :: S.ByteString) >>
	return cd { state = QuitMode }
	)

