module Main where

import Data.ByteString.Lazy
import Data.ByteString.Builder
import System.Environment
--
import Init
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
main = undefined

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

