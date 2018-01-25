{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Storage
-- Copyright   : (c) pwegrzyn, Pankrip
-- License     : MIT
-- Maintainer  : pwegrzyn
-- Stability   : stable
--
-- This is the backend storage of the nntp server. It consists of a list of currently provided groups,
-- each of which holds the information about it's name, date of creation (to create a group one needs to
-- contact the administrator of the server) and the list of articles assigned to this particular group - 
-- each article is represented interanly by it's unique Message-ID.
--

module Storage (

    -- * Working with the Storage system
    Storage(..),        -- : constructors
    updateGroups,       -- : Storage -> S.ByteString -> Maybe Storage
    findNewGroups,      -- : Storage -> DateTime -> [Group]
    getGroup,           -- : Storage -> String -> Maybe Group
    getGrpsOfArt,       -- : Storage -> Article -> [Group]
    fetchArtNumID,      -- : Storage -> String -> Int -> Maybe Article
    -- * Serialization of the Storage
    restoreStorage,     -- : String -> Maybe Storage
    syncStorage         -- : Storage -> String -> IO ()

) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Dates
import Group
import Article
import qualified Data.List as L
import Data.Tuple
import Data.Aeson

data Storage = Storage 
               {
                   groups :: [Group]    -- ^ the list of all the groups present on the server
               } deriving (Show)

-- Used in serialization
instance ToJSON Storage where
  toJSON storage = object
    [ "groups" .= toJSON (groups storage)
    ]

instance FromJSON Storage where
  parseJSON = withObject "Storage" $ \o -> do
    groups_ <- o .: "groups"
    return $ Storage groups_

-- -----------------------------------------------------------------------------
-- Working with the Storage system

-- | Given a new article, scan all the news groups found in the Newsgroups header field
-- and update the state of the Storage system in memory accordindly
updateGroups :: Storage         -- ^ the current state of the storage system
             -> S.ByteString    -- ^ the article in literal form (bytestring)
             -> Maybe Storage   -- ^ the updated version of the system after adding the article to appropriate groups
updateGroups (Storage {groups = grps}) cont = 
    getNewsgroups cont >>=
    \x -> getMessageID cont >>=
    \y -> return (Storage 
                 {
                     groups = map (\g -> if checkIfArtInGrp (Article{identifier = y}) g 
                                            then g 
                                         else 
                                            addArtToGrp (Article{identifier = y}) g) grps
                 })

-- | Finds all the groups in the storage, whose date of creation is bigger
-- than a particular date (used in NEWGROUPS)
findNewGroups :: Storage        -- ^ current state of the storage
              -> DateTime       -- ^ referential term
              -> [Group]        -- ^ list of groups created after the referential term
findNewGroups (Storage {groups = grps}) dt =
    filter (\g -> creationDT g >= dt) grps

-- | Find the group with a given name in the database. May fail if
-- no such group is present
getGroup :: Storage         -- ^ current state of the storage
         -> String          -- ^ name of the sought group
         -> Maybe Group     -- ^ found group
getGroup (Storage {groups = grps}) nameOfGrp =
    L.find (\g -> name g == nameOfGrp) grps

-- | Get a list of all groups to which a particular article belongs
getGrpsOfArt :: Storage     -- ^ current state of the storage
             -> Article     -- ^ article in question
             -> [Group]     -- ^ list of found groups
getGrpsOfArt stg art =
    filter (\g -> art `elem` (articles g)) (groups stg)

-- | Retrieve the article from a particular group with a given numeric identifer
fetchArtNumID :: Storage       -- ^ current state of the storage
              -> String        -- ^ name of the group
              -> Int           -- ^ numeric identifier of the article
              -> Maybe Article -- ^ found article
fetchArtNumID stg nameOfGrp index =
    getGroup stg nameOfGrp >>=
    \g -> getArtNumID g index

-- -----------------------------------------------------------------------------
-- Restoring and syncing the interal Storage with an external one

-- | Fetches the previously saved data from the database (filesystem) and maps it in the memory
restoreStorage :: String              -- ^ path to the location of the fs database
               -> IO (Maybe Storage)  -- ^ restored storage
restoreStorage path = 
    B.readFile (path ++ "STORAGE.json" ) >>=
    \inpJson -> return $ decode inpJson

-- | Syncs the local database storage with the current state of the server,
-- so during the next initialization of the server the previous state can be recovered
syncStorage :: Storage     -- ^ the current state of the storage
            -> String      -- ^ path for the fs database location
            -> IO ()
syncStorage storage path =
    B.writeFile (path ++ "STORAGE.json") $ encode storage