{-# LANGUAGE OverloadedStrings #-}

module MasterSocketSpec where

import Data.ByteString
import Foreign.C.Types
import Network.Socket
import Control.Concurrent.MVar
import System.IO.Unsafe
import Test.Hspec
import MasterSocket

-- because there's no such function defined in Test.Hspec.Expectations
shouldReturnSatisfy :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldReturnSatisfy` expected = action >>= (`shouldSatisfy` expected)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "MasterSocket" $ do
		describe "initMasterSocket" $ do
			it "creates new bound socket on IADDR_ANY" $ do
				initMasterSocket Nothing ("22222" :: ByteString)
				`shouldReturn`
				MkSocket (25 :: CInt) AF_INET Stream (6 :: CInt) (unsafePerformIO $ newMVar Bound)
			it "or on specified address" $ do
				initMasterSocket (Just ("127.0.0.1" :: ByteString)) ("33333" :: ByteString)
				`shouldReturn`
				MkSocket (26 :: CInt) AF_INET Stream (6 :: CInt) (unsafePerformIO $ newMVar Bound)

