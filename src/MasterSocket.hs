{-# LANGUAGE BangPatterns #-}

module MasterSocket
	( initMasterSocket
	, acceptLoop
	) where

import qualified Data.ByteString as S hiding (unpack)
import qualified Data.ByteString.Char8 as Cs
import qualified Data.Word as W
import qualified Data.HashSet as H
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as N
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Control.Monad as M
import System.Posix.Types (Fd (..))
import Debug.Trace
import qualified System.IO.Unsafe as EV
import qualified State as ST
import qualified Control.Concurrent.QSemN as SM
--
import Signals (ShutdownSignal (..), sendTermSignal, ClientFinishedSignal (..))
import Client (clientThread, threadFinish)

-- | standard TCP protocol number as defined by IANA
pROTO_TCP = 6 :: N.ProtocolNumber

-- | Creates a 'Socket' and binds it to specified address and port
initMasterSocket :: (Maybe S.ByteString) -- ^ hostname to bind to. Nothing for 'iNADDR_ANY'
		 -> S.ByteString -- ^ string with port number or service name
		 -> IO N.Socket -- ^ newly created bound socket (not in listening state)
initMasterSocket host port = (
	let
	hints = N.AddrInfo
		{ N.addrFlags      = [N.AI_PASSIVE]
		, N.addrFamily     = N.AF_INET
		, N.addrSocketType = N.Stream
		, N.addrProtocol   = pROTO_TCP
		}
	in
	N.getAddrInfo (Just hints) (Cs.unpack <$> host) (Just (Cs.unpack port :: N.ServiceName)) >>=
	\(addr:_) -> (
		N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr) >>=
		\sock -> (
			N.bind sock (N.addrAddress addr) >>
			return sock
		)
	)
	)

-- | Main server loop, listens for incoming connections and dispatches them to client threads
acceptLoop :: N.Socket -- ^ 'Socket' to listen on
	   -> Int -- ^ Maximum number of waiting connections, also, 'Socket'\'s _backlog_ parameter
	   -> IO ()
acceptLoop sock backlog = (
	-- for testing purposes
	N.setSocketOption sock N.ReuseAddr 0 >>
	N.listen sock backlog >>
	let
	fd = N.fdSocket sock
	pending = H.empty :: H.HashSet C.ThreadId
	in
	-- TODO: log: accepting connections..
	servloop sock (Fd fd) backlog backlog pending
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
		C.myThreadId >>=
		\mainThreadId -> (
			E.catches (
				-- if there's pending connection then the socket's file descriptor is available for read
				C.threadWaitRead fd >>
				N.accept s >>=
				\(csock, caddr) -> (
					-- TODO: log caddr
					{-
					C.forkFinally $ (clientThread csock)
							(\_ -> myThreadId >>=
							\mtid -> throwTo mainThreadId (ClientFinished mtid)
					) >>=
					-}
					C.forkFinally (clientThread csock) (threadFinish mainThreadId) >>=

					\tid -> (
						 (traceIO "registering new client\n")
						 -- >> (ST.semaphore >>= \sem -> SM.signalQSemN sem 1)
						 >>= \c -> return $ H.insert tid pending
					)
				) >>=
				-- point free for \set -> servloop s fd set
				servloop s fd maxconns (conns - 1)
				) [
				E.Handler (\ShutdownSignal -> (
						traceIO "recieved exit signal\n" >>
						gracefulExit pending >>
						wrapUp s
						)
					)
				, E.Handler (
					\(ClientFinishedSignal tid) -> (
						servloop s fd maxconns (
							-- avoid overflow
							if conns < maxconns then
								conns + 1
							else maxconns
							) (H.delete tid pending)
							-- >> ST.semaphore >>= \sem -> SM.signalQSemN sem (-1)
						)
				)
				]
			)
	)

-- | Traverses 'H.HashSet' of open connections and instructs them to finish
{-# NOINLINE gracefulExit #-}
gracefulExit :: H.HashSet C.ThreadId -> IO ()
gracefulExit pending = (
	if H.null pending then
		-- nothing to do exit immediately
		traceIO "empty pending connections set\n" >>
		return () :: IO ()
	else
		let
		conns = H.toList pending
		in
		-- ignore return value (which is not important) for a nice function signature
		-- (\_ -> ()) $ foldr1 (\tid _ -> sendTermSignal tid) conns
		(traceIO $ "there are " ++ (show $ length conns) ++ " waiting to be closed\n") >>
		M.foldM (\_ tid -> sendTermSignal tid >>= \(!v) -> return ()) () conns
		-- >> ST.semaphore >>= \sem -> SM.waitQSemN sem 0
	)

-- | close the master 'Socket'
wrapUp :: N.Socket -> IO ()
wrapUp s = (
	N.shutdown s N.ShutdownBoth >>
	N.close s
	)

