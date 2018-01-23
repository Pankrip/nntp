{-# LANGUAGE DeriveDataTypeable #-}

module Signals
	( installExitHandlers
	, sendTermSignal
	, ShutdownSignal (..)
	, TerminateClientSignal (..)
	, ClientFinishedSignal (..)
	) where

import qualified Control.Exception as E
import qualified Control.Concurrent as C
import qualified Data.Typeable as T
import qualified System.Posix.Signals as S

data ShutdownSignal = ShutdownSignal deriving (Show, Eq)
instance E.Exception ShutdownSignal

data TerminateClientSignal = TerminateClientSignal deriving (Show, Eq)
instance E.Exception TerminateClientSignal

newtype ClientFinishedSignal = ClientFinishedSignal C.ThreadId deriving (Show, Eq, T.Typeable)
instance E.Exception ClientFinishedSignal

-- | Signal handler invoked upon reception of SIGINT, SIGTERM or SIGQUIT
exitHandler :: C.ThreadId -> S.Handler
exitHandler tid = S.Catch (
	C.throwTo tid ShutdownSignal
	)

-- | Installs exit handlers to gracefully terminate server operation
installExitHandlers :: C.ThreadId
		    -> IO ()
installExitHandlers tid = (
	S.installHandler S.sigTERM (exitHandler tid) (Just S.fullSignalSet) >>
	S.installHandler S.sigQUIT (exitHandler tid) (Just S.fullSignalSet) >>
	S.installHandler S.sigINT (exitHandler tid) (Just S.fullSignalSet) >>
	return ()
	)

-- | Sends a termination signal to client handler
sendTermSignal :: C.ThreadId -> IO ()
sendTermSignal tid = (
	C.throwTo tid TerminateClientSignal
	)

