-- |
-- Module      : Group
-- Copyright   : (c) pwegrzyn, Pankrip
-- License     : MIT
-- Maintainer  : none
-- Stability   : stable
--
-- This module represents a particular instance of a new newsgroup.
--

module Group (

    -- * Working with Groups
    Group(..),       -- this is only exported for testing purpouses
    mkNewGroup,      -- : String -> DateTime -> Group
    addArtToGrp,     -- : Article -> Group -> Group
    checkIfArtInGrp, -- : Article -> Group -> Bool
    getNumericsForGrp-- : Group -> (Int, Int, Int)

) where

-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString as S
-- import Text.Regex.Posix
-- import qualified Data.ByteString.Char8 as C
-- import Data.List.Split (splitOn)
-- import Data.Char (toUpper)
-- import qualified Data.Text as T
-- import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.Dates
import Article

-- | ADT constructor takes a DateTime representing the date and time of the creation
-- of the group as well as the list of articles associated with the group and the name
-- of the group
data Group = Group
             {
                 name         :: String,      -- ^ name of the group
                 creationDT   :: DateTime,    -- ^ date and time of creation    
                 articles     :: [Article]    -- ^ list of articles of a group
             } deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- Working with Groups

-- | Creates a new group in the system
mkNewGroup :: String    -- ^ name of the group
           -> DateTime  -- ^ date and time of creation
           -> Group     -- ^ new-made group
mkNewGroup gName dt = Group
                      {
                        name = gName,
                        articles = [],
                        creationDT = dt
                      }

-- | Adds a new article to a particular group
addArtToGrp :: Article  -- ^ new article to be added 
            -> Group    -- ^ the group to which the article should be added
            -> Group    -- ^ the group with the newly-added article
addArtToGrp art (Group {name = n, articles = a, creationDT = c}) = 
    Group 
    {
        name = n,
        creationDT = c,
        articles = (art : a)
    }

-- | Checks whether the article is already in group or not
checkIfArtInGrp :: Article  -- ^ the article to be checked for inclusion
                -> Group    -- ^ the group in question
                -> Bool     -- ^ information about the inclusion
checkIfArtInGrp art (Group {name = _, articles = a, creationDT = _}) =
    if art `elem` a then True else False

-- | Retrieves the numeric identifiers of the first and last article in a group
-- as well as the number of articles in a group
getNumericsForGrp :: Group              -- ^ the group in question
                  -> (Int, Int, Int)    -- ^ (first, last, amount)
getNumericsForGrp (Group {name = n, articles = a, creationDT = c}) =
    let
        len = length a
    in
        (0, if len == 0 then len else len - 1 , len)