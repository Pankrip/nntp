{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Group
-- Copyright   : (c) pwegrzyn, Pankrip
-- License     : MIT
-- Maintainer  : pwegrzyn
-- Stability   : stable
--
-- This module represents a particular instance of a new newsgroup.
--

module Group (

    -- * Working with Groups
    Group(..),           -- this is only exported for testing purpouses
    mkNewGroup,          -- : String -> DateTime -> Group
    addArtToGrp,         -- : Article -> Group -> Group
    checkIfArtInGrp,     -- : Article -> Group -> Bool
    getNumericsForGrp,   -- : Group -> (Int, Int, Int)
    checkIfValidNumeric, -- : Group -> Int -> Bool
    getArtNumID,         -- : Group -> Index -> Maybe Article
    getNumericID,        -- : Article -> Group -> Maybe Int
    getNextArt,          -- : Group -> Article -> Maybe Article
    getPrevArt           -- : Group -> Article -> Maybe Article

) where

import Data.Dates
import Article
import Data.Aeson
import qualified Data.List as L

-- | ADT constructor takes a DateTime representing the date and time of the creation
-- of the group as well as the list of articles associated with the group and the name
-- of the group
data Group = Group
             {
                 name         :: String,      -- ^ name of the group
                 creationDT   :: DateTime,    -- ^ date and time of creation    
                 articles     :: [Article]    -- ^ list of articles of a group
             } deriving (Show, Eq)

-- Used in serialization
instance ToJSON Group where
  toJSON group = object
    [ "name" .= toJSON (name group)
    , "creationDT" .= toJSON (creationDT group)
    , "articles" .= toJSON (articles group)
    ]

instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    name_ <- o .: "name"
    creationDT_ <- o .: "creationDT"
    articles_ <- o .: "articles"
    return $ Group name_ creationDT_ articles_

instance ToJSON DateTime where
  toJSON dt = object
    [ "year" .= toJSON (year dt)
    , "month" .= toJSON (month dt)
    , "day" .= toJSON (day dt)
    , "hour" .= toJSON (hour dt)
    , "minute" .= toJSON (minute dt)
    , "second" .= toJSON (second dt)
    ]

instance FromJSON DateTime where
  parseJSON = withObject "DateTime" $ \o -> do
    year_ <- o .: "year"
    month_ <- o .: "month"
    day_ <- o .: "day"
    hour_ <- o .: "hour"
    minute_ <- o .: "minute"
    second_ <- o .: "second"
    return $ DateTime year_ month_ day_ hour_ minute_ second_

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

-- | Check if numeric identifer is valid for a group
-- (i. e. there exists an article in the group with such numeric ID)
checkIfValidNumeric :: Group    -- ^ given group
                    -> Int      -- ^ numeric identifier of an article in a group
                    -> Bool     -- ^ iformation whether the identifer is valid or not
checkIfValidNumeric grp index =
    index >= (fst3 (getNumericsForGrp grp)) && index <= (snd3 (getNumericsForGrp grp))

-- | Get the article belonging to a paricular group which has a given numeric identifer
getArtNumID :: Group          -- ^ group in question
            -> Int            -- ^ numeric identifier of the sought article
            -> Maybe Article  -- ^ found article
getArtNumID grp numID =
    if checkIfValidNumeric grp numID
        then Just $ (articles grp) !! numID
    else
        Nothing

-- | Given an article and a group get the numeric id of an article in the group
getNumericID :: Article
             -> Group
             -> Maybe Int      -- ^ Just the found numeric ID or nothing if no such article has been found in this group
getNumericID art grp = 
    go art grp 0
        where go :: Article -> Group -> Int -> Maybe Int
              go article group acc = 
                case article == ((articles group) !! acc) of
                    True -> Just acc
                    _    -> if acc == length (articles group)
                                then Nothing
                            else
                                go article group $ acc + 1

-- | Given a group and an article get the next article in the group
getNextArt :: Group             
           -> Article
           -> Maybe Article     -- ^ Just the next article or nothing if there's no next article in the group
getNextArt grp art = 
    getNumericID art grp >>=
    \found -> (if checkIfValidNumeric grp $ found + 1
                then Just found
              else 
                Nothing) >>=
              \next -> return $ (articles grp) !! next

-- | Given a group and an article get the previous article in the group
getPrevArt :: Group             
           -> Article
           -> Maybe Article     -- ^ Just the previous article or nothing if there's no previous article in the group
getPrevArt grp art = 
    getNumericID art grp >>=
    \found -> (if checkIfValidNumeric grp $ found - 1
                then Just found
              else 
                Nothing) >>=
              \prev -> return $ (articles grp) !! prev

-- -----------------------------------------------------------------------------
-- Helper functions

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x