module Test.Helpers
	( sockPair
	, readingSocket
	, shouldReturnSatisfy
	) where

import qualified Data.ByteString as S
import qualified System.Posix.Types as T
import qualified Network.Socket as N hiding (send, recv)
import qualified Network.Socket.ByteString as N
import Test.Hspec

-- because there's no such function defined in Test.Hspec.Expectations
shouldReturnSatisfy :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldReturnSatisfy` expected = action >>= (`shouldSatisfy` expected)

sockPair :: IO (N.Socket, N.Socket)
sockPair = (
	N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
	)

readingSocket :: S.ByteString -> IO N.Socket
readingSocket str = (
	sockPair >>=
	\(r, w) -> (
		N.send w str >>
		return r
		)
	)

