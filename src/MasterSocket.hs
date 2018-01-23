module MasterSocket
	( initMasterSocket
	, acceptLoop
	) where

import Data.ByteString hiding (unpack)
import Data.ByteString.Char8
import Data.Word
import qualified Data.HashSet as H
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as N
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import System.Posix.Types (Fd)
--
import Signals (ShutdownSignal, sendTermSignal, ClientFinished)
import Client (clientThread)

-- | standard TCP protocol number as defined by IANA
pROTO_TCP = 6 :: ProtocolNumber

-- | Creates a 'Socket' and binds it to specified address and port
initMasterSocket :: (Maybe ByteString) -- ^ hostname to bind to. Nothing for 'iNADDR_ANY'
		 -> ByteString -- ^ string with port number or service name
		 -> IO Socket -- ^ newly created bound socket (not in listening state)
initMasterSocket host port = (
	let
	hints = N.AddrInfo
		{ addrFlags      = [N.AI_PASSIVE]
		, addrFamily     = N.AF_INET
		, addrSocketType = N.Stream
		, addrProtocol   = pROTO_TCP
		}
	in
	N.getAddrInfo (Just hints) (unpack <$> host) (Just (unpack port :: ServiceName)) >>=
	\(addr:_) -> (
		N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr) >>=
		\sock -> (
			N.bind sock (N.addrAddress addr) >>
			return sock
		)
	)
	)

-- | Main server loop, listens for incoming connections and dispatches them to client threads
acceptLoop :: Socket -- ^ 'Socket' to listen on
	   -> Int -- ^ Maximum number of waiting connections, also, 'Socket'\'s _backlog_ parameter
	   -> IO ()
acceptLoop sock backlog = (
	-- for testing purposes
	N.setSocketOption sock N.ReuseAddr 0 >>
	N.listen sock backlog >>
	let
	fd = N.fdSocket sock
	pending = empty :: H.HashSet C.ThreadId
	in
	-- TODO: log: accepting connections..
	servloop s fd backlog backlog pending
	)

-- | Inner tail-recursive loop with "local variables"
servloop :: N.Socket -- ^ master socket
	 -> Fd -- ^ master socket\'s file descriptor
	 -> Int -- ^ maximum number of allowed connections
	 -> Int -- ^ number of open connection slots left
	 -> H.HashSet C.ThreadId -- ^ 'H.HashSet' with 'C.ThreadId'\'s of all active connections
	 -> IO ()
servloop s fd maxconns conns pending = (
	if conns <= 0 then
		-- maximum open connections limit reached
		-- TODO
		undefined
	else
		-- interrupt operation when shutdown signal is recieved
		E.handle (\ex ->
			case ex of
				ShutdownSignal -> (
					let
					-- poor man's sequencing of pure functions
					_ = gracefulExit pending
					in
					wrapUp s
				)
				ClientFinished tid -> (
					servloop s fd maxconns (
						-- avoid overflow
						if cons < maxconns then
							cons + 1
						else maxconns
						) (H.delete tid conns)
				)
			) (
			-- if there's pending connection then the socket's file descriptor is available for read
			C.threadWaitRead fd >>
			N.accept s >>=
			\(csock, caddr) -> (
				-- TODO: log caddr
				C.forkFinally $ (clientThread csock)
						(\_ -> myThreadId >>=
						\mtid -> throwTo mainThreadId (ClientFinished mtid)
				) >>=
				\tid -> (
				return $ H.insert tid pending
				)
			) >>=
			-- point free for \set -> servloop s fd set
			servloop s fd maxconns (conns - 1)
			)
	)

-- | Traverses 'H.HashSet' of open connections and instructs them to finish
gracefulExit :: H.HashSet C.ThreadId -> IO ()
gracefulExit pending = (
	if null pending then
		-- nothing to do exit immediately
		return () :: IO ()
	else
		let
		conns = H.toList pending
		in
		-- ignore return value (which is not important) for a nice function signature
		-- (\_ -> ()) $ foldr1 (\tid _ -> sendTermSignal tid) conns
		foldM (\_ tid -> sendTermSignal tid) conns
	)

-- | close the master 'Socket'
wrapUp :: N.Socket -> IO ()
wrapUp s = (
	shutdown s ShutdownBoth >>
	close s
	)

