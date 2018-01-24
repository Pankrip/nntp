module Main where

import qualified Data.ByteString.Lazy as L
-- import Data.ByteString.Builder as B
import qualified System.Environment as E
import qualified Control.Concurrent as C
--
import Init
import Options
import Signals
import MasterSocket

main :: IO ()
{-
-- TODO: Either + IO
main = (
	getArgs >>=
	\argv -> getConfig argv >>=
	\conf -> initMasterSocket (host conf) (port conf) >>=
	\msock -> undefined
	)
-}
--main = undefined

{-
 - main() components:
 - getArgs
 - getConfig
 - initMasterSocket
 - installExitHandlers
 - ResotreStorage
 - MasterLoop
 - SyncStorage
 - exit
-}

-- proof of concept testing
main = (
	E.getArgs >>=
	return . getConfig >>=
	proceedOrFail
	)

proceedOrFail :: Either String Options -> IO ()
proceedOrFail (Left str) = putStrLn str
proceedOrFail (Right conf) = (
	case (port conf) of
		Nothing -> putStrLn "empty port"
		Just p -> (
			initMasterSocket (L.toStrict <$> (host conf)) (L.toStrict p) >>=
			\sock -> (
				C.myThreadId >>= installExitHandlers >>
				acceptLoop sock 5
				)
			)
	)

