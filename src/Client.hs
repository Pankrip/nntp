{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Client
	(
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

-- | maximum size of incoming client data NNTP standard permits
-- (applicable only in 'CommandMode')
maxQUERY = 510 :: T.ByteCount

-- | stadard network newline
newLine = "\r\n" :: S.ByteString
-- | sequence used to indicate end of post, as specified by standard
postEndSequence = "\r\n.\r\n" :: S.ByteString

-- | Datatype indicating the state client is in
data OperationMode = CommandMode -- ^ indicates that client is sending regular commands
		   | PostMode -- ^ indicates that client is uploading a post (eg. after a POST or IHAVE command)
		   deriving (Eq, Show)

-- | REPL loop for single client connection
-- responsible for dispatching NNTP commands
clientThread :: N.Socket -- ^ 'N.Socket' for communication with client
	     -> IO ()
clientThread cs = (
	let
	sockfd = N.fdSocket cs
	in
	E.handle (\Sig.TerminateClientSignal -> gracefulExit cs)
		 (repl (T.Fd sockfd) (L.empty :: L.ByteString) maxQUERY CommandMode)
	)

-- |
repl :: T.Fd
     -> L.ByteString
     -> T.ByteCount {- ^ serves double purpose - in 'CommandMode' indicates how much space in buffer is left
    		       in 'PostMode' indicates total length of the string
		  -}
     -> OperationMode
     -> IO ()
repl fd buf max isPosting = (
	C.threadWaitRead fd >>
	-- this one extra byte is used to determine if the sent command isn't too long
	I.fdRead fd (max + 1) >>=
	-- TODO: rewrite using I.fdReads
	\(str, count) -> (
		case isPosting of
			CommandMode -> (
				-- TODO: handle the case when \r arrives in one packet and \n in another
				case S.breakSubstring newLine str of
				(h, t) -> (
					if S.null h then
						-- TODO dispatch command and collect buffer
						-- TODO dispatch() (returns mode and whether to quit or not
						repl fd (L.fromStrict t) (strlen t) undefined
					else
						if S.null t then
							-- TODO just dispatch the command
							undefined
						else
							-- no newline in the message
							-- TODO
							if max > count then
								-- not if >= because if there's already
								-- 512 chars and no \r\n then it's too long
								repl fd (L.append buf (L.fromStrict str))
									(max - count) CommandMode
							else
								-- TODO: send 50x message to client
								-- (too long command)
								undefined
					)
				)
			PostMode -> (
				-- TODO: handle the case when \r\n.\r\n arrives in multiple packets
				case S.breakSubstring postEndSequence str of
				(h, t) -> (
					if S.null h then
						-- TODO finish post and start command buffer
						undefined
					else
						if S.null t then
							-- TODO just finish post and start fres read
							undefined
						else
							-- post didn't end yet
							repl fd (L.append buf (L.fromStrict str))
								(max + count) PostMode
					)
				)
	)
	)

-- | Sends a 400 Service Discountiued message to client and closes the connection
gracefulExit :: N.Socket
	     -> IO ()
gracefulExit cs = undefined

-- | helper function to return size with 'T.ByteCount' type
strlen :: S.ByteString
       -> T.ByteCount
strlen s = (Ct.CSize (fromInteger (fromIntegral S.length s :: Integer)))

