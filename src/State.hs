module State
	( getStorageLocation
    , putStorageLocation
    , getStorage
    , putStorage
--	, strictIO
	) where

import qualified Control.Concurrent.MVar as M
import qualified Data.ByteString.Lazy as L
-- import qualified System.IO as I
-- import qualified System.IO.Strict as ST
import qualified Storage as S

storageLocation :: IO (M.MVar L.ByteString)
storageLocation = M.newEmptyMVar

getStorageLocation :: IO L.ByteString
getStorageLocation = storageLocation >>= M.takeMVar

putStorageLocation :: L.ByteString -> IO ()
putStorageLocation s = storageLocation >>= \st -> M.putMVar st s

storage :: IO (M.MVar S.Storage)
storage = M.newEmptyMVar

getStorage :: IO S.Storage
getStorage = storage >>= M.takeMVar

putStorage :: S.Storage -> IO ()
putStorage s = storage >>= \st -> M.putMVar st s

{-
forceExecution :: IO I.Handle
forceExecution = (ST.openFile "/dev/null" I.ReadWriteMode) >>= M.newMVar

strictIO :: ST.SIO Char
strictIO = forceExecution >>= M.takeMVar >>= ST.hGetChar >>= ST.return'
-}