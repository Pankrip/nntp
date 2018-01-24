{-# LANGUAGE OverloadedStrings #-}

module MasterSocketSpec where

import Data.ByteString
import Foreign.C.Types
import qualified Network.Socket as N
import Control.Concurrent.MVar
import System.IO.Unsafe

import Test.Hspec
import Test.Helpers
import MasterSocket

compareSocket :: Maybe String -> String -> N.Socket -> Bool
compareSocket Nothing port s = compareSocket
	(Just $ unsafePerformIO $ N.inet_ntoa N.iNADDR_ANY) port s
compareSocket (Just testhost) port s = unsafePerformIO $
	 N.isBound s >>=
	 \isB -> N.getSocketName s >>=
	 \(N.SockAddrInet num host) -> (
		return $ and [ isB
			, num == (read port :: N.PortNumber)
			, host == (unsafePerformIO $ N.inet_addr testhost)
		]
		)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "MasterSocket" $ do
		describe "initMasterSocket" $ do
			it "creates new bound socket on IADDR_ANY" $ do
				initMasterSocket Nothing ("22222" :: ByteString)
				`shouldReturnSatisfy`
				(compareSocket Nothing "22222")
				-- MkSocket (25 :: CInt) AF_INET Stream (6 :: CInt) (unsafePerformIO $ newMVar Bound)
			it "or on specified address" $ do
				initMasterSocket (Just ("127.0.0.1" :: ByteString)) ("33333" :: ByteString)
				`shouldReturnSatisfy`
				(compareSocket (Just "127.0.0.1") "33333")
				-- MkSocket (26 :: CInt) AF_INET Stream (6 :: CInt) (unsafePerformIO $ newMVar Bound)

