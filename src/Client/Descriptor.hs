module Client.Descriptor
	( ClientDescriptor (..)
	, newClientDescriptor
	, OperationMode (..)
	) where

import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as N
--
import qualified Group as G
import qualified Article as A

-- | structure describing client state
data ClientDescriptor = ClientDescriptor
	{ state :: OperationMode -- ^ state of the state machine of client connection
	, socket :: N.Socket -- ^ socket used for communication with client
	, authinfo :: Maybe L.ByteString -- ^ 'Just' username of authenticated client, or 'Nothing'
	, lastCmd :: Maybe L.ByteString -- ^ last executed command (used to determine which command text body belongs to)
	, group :: Maybe G.Group -- ^ current selected group
	, article :: Maybe A.Article -- ^ current article pointer
	} deriving (Show, Eq)


-- | Returns 'ClientDescriptor' for fresh connection
newClientDescriptor :: N.Socket -> ClientDescriptor
newClientDescriptor sock = ClientDescriptor
	{ state = CommandMode
	, socket = sock
	, authinfo = Nothing
	, lastCmd = Nothing
	, group = Nothing
	, article = Nothing
	}

-- | Datatype indicating the state client is in
data OperationMode = CommandMode -- ^ indicates that client is sending regular commands
		   | PostMode -- ^ indicates that client is uploading a post (eg. after a POST or IHAVE command)
		   | QuitMode -- ^ indicates that server must terminate the connection
		   deriving (Eq, Show)
