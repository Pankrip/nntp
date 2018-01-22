-- |
-- Module      : Storage
-- Copyright   : (c) pwegrzyn, Pankrip
-- License     : MIT
-- Maintainer  : none
-- Stability   : stable
--
-- This is the backend storage of the nntp server.
--

module Storage (

    -- * Working with the Storage system
    Storage

) where

-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString as S
-- import Text.Regex.Posix
-- import qualified Data.ByteString.Char8 as C
-- import Data.List.Split (splitOn)
-- import Data.Char (toUpper)
-- import qualified Data.Text as T
-- import Data.Dates
import Group

data Storage = Storage 
               {
                   groups :: [Group]    -- ^ the list of all the groups present on the server
               }

-- -----------------------------------------------------------------------------
-- Working with the Storage system

