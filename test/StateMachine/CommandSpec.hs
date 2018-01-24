{-# LANGUAGE OverloadedStrings #-}

module StateMachine.CommandSpec where

import Test.Hspec
import StateMachine.Command

import Test.Helpers
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as N hiding (send, recv)
import qualified Network.Socket.ByteString as N
import System.Posix.Types (Fd (..), ByteCount (..))

import qualified Client.Internal as C

cleanLine = ("testing\r\n" :: S.ByteString)
returnLine = ("goodbye!\r\n" :: L.ByteString)
maxLen = 512 :: ByteCount

sock :: IO N.Socket
sock = readingSocket cleanLine

main :: IO ()
main = hspec spec

spec :: Spec
-- spec = undefined
spec = beforeAll (
	sockPair
	) (do {
	describe "State.Machine.Client" $ do
		describe "parses incoming data into discrete lines" $ do
			it (show cleanLine) $ \(rs, ws) -> do
				N.sendAll ws cleanLine
				csm START (Fd $ N.fdSocket rs) L.empty undefined maxLen (C.newClientDescriptor rs)
				`shouldReturn`
				((C.newClientDescriptor rs) { C.state = C.QuitMode }, L.empty);
	}
	)
{--}


{-
---------------------------------------

-- mockup for state machine
module Client (dispatchCommand) where

dispatchCommand :: L.ByteString -> ClientState
-}
