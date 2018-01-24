{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Client
	( OperationMode (..)
	, ClientDescriptor (..)
	, clientThread
	, threadFinish
	) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Network.Socket as N
import qualified "unix-bytestring" System.Posix.IO.ByteString as I
import qualified System.Posix.Types as T
import qualified Foreign.C.Types as Ct
import qualified Data.Word as W
--
-- import Signals (TerminateClientSignal)
-- can't import instances with named import
import qualified Signals as Sig
import Client.Internal
import qualified StateMachine.Command as CSM

-- | maximum size of incoming client data NNTP standard permits
-- (applicable only in 'CommandMode')
maxQUERY = 512 :: T.ByteCount

-- | sequence used to indicate end of post, as specified by standard
postEndSequence = "\r\n.\r\n" :: S.ByteString

-- | main REPL loop for single client connection
-- responsible for dispatching NNTP commands
clientThread :: N.Socket -- ^ 'N.Socket' for communication with client
	     -> IO ()
clientThread cs = (
	let
	sockfd = N.fdSocket cs
	in
	E.handle (\Sig.TerminateClientSignal -> gracefulExit cs)
		 -- (repl (T.Fd sockfd) (L.empty :: L.ByteString) maxQUERY CommandMode)
		 (repl CommandMode L.empty (newClientDescriptor cs))
	)

-- | function that runs on thread destruction and informs main thread that it has finished
threadFinish :: C.ThreadId -- ^ thread ID of thread which will be notified
	    -> Either E.SomeException () -- ^ reason of thread termination
	    -> IO ()
-- threadFinish Left ex =
-- threadFinish Right _ = (
threadFinish tid _ = (
	C.myThreadId >>=
	\mtid -> C.throwTo tid (Sig.ClientFinishedSignal mtid)
	)


-- | Sends a 400 Service Discountiued message to client and closes the connection
gracefulExit :: N.Socket
	     -> IO ()
gracefulExit cs = undefined

{-
-- | helper function to return size with 'T.ByteCount' type
strlen :: S.ByteString
       -> T.ByteCount
strlen s = (Ct.CSize (fromInteger (fromIntegral S.length s :: Integer)))
-}

----------------------- REPL -----------------------------
-- | Read Eval Print Loop of single client connection
repl :: OperationMode -- ^ operation state to transition into
     -> L.ByteString -- ^ buffer
     -> ClientDescriptor
     -> IO ()

repl CommandMode buf cd = (
	-- CSM.csm CSM.START (T.Fd (N.fdSocket $ socket cd)) buf undefined maxQUERY cd
	CSM.csm CSM.START (T.Fd (N.fdSocket $ socket cd)) buf undefined maxQUERY cd >>=
	\(newstate, buf) -> (
		repl (state newstate) buf newstate
		)
	)

repl PostMode buf cd = (
	-- finishPost buf >>=
	-- always transition into CommandMode after recieving post
	-- CSM.csm CSM.START (N.fdSocket $ socket cd) buf undefined maxQUERY cd
	-- TODO
	undefined
	)

repl QuitMode _ cd@(ClientDescriptor { socket = s }) = (
	N.shutdown s N.ShutdownBoth >>
	N.close s
	)
----------------------- END OF REPL -----------------------------

