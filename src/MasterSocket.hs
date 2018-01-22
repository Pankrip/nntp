module MasterSocket
	( initMasterSocket
	) where

import Data.ByteString hiding (unpack)
import Data.ByteString.Char8
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom, recvLen)
import Network.Socket.ByteString

-- | standard TCP protocol number as defined by IANA
pROTO_TCP = 6 :: ProtocolNumber

-- | Creates a 'Socket' and binds it to specified address and port
initMasterSocket :: (Maybe ByteString) -- ^ hostname to bind to. Nothing for 'iNADDR_ANY'
		 -> ByteString -- ^ string with port number or service name
		 -> IO Socket -- ^ newly created bound socket (not in listening state)
initMasterSocket host port = (
	let
	hints = AddrInfo
		{ addrFlags      = [AI_PASSIVE]
		, addrFamily     = AF_INET
		, addrSocketType = Stream
		, addrProtocol   = pROTO_TCP
		}
	in
	getAddrInfo (Just hints) (unpack <$> host) (Just (unpack port :: ServiceName)) >>=
	\(addr:_) -> (
		socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) >>=
		\sock -> (
			bind sock (addrAddress addr) >>
			return sock
		)
	)
	)

