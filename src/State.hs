module State
	( getStorageLocation
	, putStorageLocation
--	, strictIO
--	, semaphore
	) where

import qualified Control.Concurrent.MVar as M
import qualified Data.ByteString.Lazy as L
import qualified System.IO as I
import qualified System.IO.Strict as ST
import qualified Control.Concurrent.QSemN as SM
--

storageLocation :: IO (M.MVar L.ByteString)
storageLocation = M.newEmptyMVar

getStorageLocation :: IO L.ByteString
getStorageLocation = storageLocation >>= M.takeMVar

putStorageLocation :: L.ByteString -> IO ()
putStorageLocation s = storageLocation >>= \st -> M.putMVar st s

{-
forceExecution :: IO (M.MVar I.Handle)
forceExecution = (ST.openFile "/dev/null" I.ReadWriteMode) >>= M.newMVar

strictIO :: IO Char
strictIO = forceExecution >>= M.takeMVar >>= I.hGetChar
---}

{-
semaphore :: IO SM.QSemN
semaphore = SM.newQSemN 0
---}

