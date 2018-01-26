{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Article
-- Copyright   : (c) pwegrzyn, Pankrip
-- License     : MIT
-- Maintainer  : pwegrzyn
-- Stability   : stable
--
-- This module represents a single article in the database.
--

module Article (

    -- * Working with Articles
    Article(..),
    getContent,          -- : Article -> B.ByteString -> IO S.ByteString
    saveContentP,        -- : S.ByteString -> String -> IO ()
    saveContentI,        -- : S.ByteString -> IO ()
    -- * Working with ByteStrings
    getHeaderStr,        -- : S.ByteString -> Maybe S.ByteString
    getBodyStr,          -- : S.ByteString -> Maybe S.ByteString
    getFieldLabels,      -- : S.ByteString -> [String]
    headerToTuples,      -- : S.ByteString -> [(S.ByteString, S.ByteString)]
    checkRequiredFields, -- : S.ByteString -> [String] -> Bool
    newsgroupsToList,    -- : (S.ByteString, S.ByteString) -> Maybe [String]
    requiredForPOST,     -- : [String]
    requiredForIHAVE,    -- : [String]
    getMessageID,        -- : Maybe String
    getFieldTuple,       -- : S.ByteString -> String -> (S.ByteString, S.ByteString)
    getHeaderStrUnsafe,  -- : S.ByteString -> S.ByteString
    getBodyStrUnsafe,    -- : S.ByteString -> S.ByteString
    getNewsgroups        -- : S.ByteString -> Maybe [String]

) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import Text.Regex.Posix
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.List.Split (splitOn)
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Dates
import System.Environment
import System.IO
import Data.Tuple
import qualified Data.List as L
import Data.Aeson
import GHC.Generics

-- | ADT constructor takes a String representing the identifier of a particular
-- article in the system's database.
data Article = Article
             {
                 identifier :: String   -- ^ the MessageID identifer of the aritcle (also the name of the file)
             } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Used for serialization
-- instance FromJSON Article
-- instance ToJSON Article

-- -----------------------------------------------------------------------------
-- Working with Articles

-- | Fetch the contents of an article from the database in the form of a ByteString
getContent :: Article               -- ^ the article in question
           -> B.ByteString          -- ^ storage location
           -> IO S.ByteString       -- ^ just the contents of the article or nothing in case of failure
getContent (Article identifier) location = do
    inHdlr <- openFile ((CL.unpack location) ++ "/" ++ identifier) ReadMode
    inpStr <- S.hGetContents inHdlr
    return inpStr

-- These next two functions reaaally need some monad transformers love (they work tho)
-- | Saves an article to the database of the server (used in POST)
saveContentP :: S.ByteString     -- ^ the contents of the article
             -> String           -- ^ the new messageid for the article
             -> IO ()
saveContentP cont iden = return (cont, iden) >>=
    \(x,y) -> return (getHeaderStr x, "\nMessage-ID: <" ++ iden ++ ">\n", getCurrentDateTime) >>=
    \(x1,y1,z1) -> z1 >>= \z2 -> return ("Date: " ++ (show z2) ++ "\n\n") >>=
    \z3 -> case x1 of 
            Nothing -> return ()
            Just header -> case getBodyStr x of
                            Nothing -> return ()
                            Just body -> S.writeFile ("database/" ++ iden) (C.pack $ C.unpack header ++ y1 ++ z3 ++ (C.unpack body))

-- | Saves an article to the database of the server (used in IHAVE)
saveContentI  :: S.ByteString     -- ^ the contents of the article
              -> IO ()
saveContentI cont =
    return (getHeaderStr cont, getCurrentDateTime) >>=
    \(x1,y1) -> y1 >>= \y2 -> return ("\nDate: " ++ (show y2) ++ "\n\n") >>=
    \y3 -> case x1 of 
            Nothing -> return ()
            Just header -> case getBodyStr cont of
                            Nothing -> return ()
                            Just body -> case getMessageID cont of
                                Nothing -> return ()
                                Just iden -> S.writeFile ("database/" ++ iden) (C.pack $ C.unpack header ++ y3 ++ (C.unpack body))

-- -----------------------------------------------------------------------------
-- Working with ByteStrings

-- | Retrieve the header as ByteString from an article in the form of a ByteString,
-- may fail if the format of the article is not conforming to the standard
getHeaderStr :: S.ByteString        -- ^ the article in the from of a ByteString 
             -> Maybe S.ByteString  -- ^ just the retrieved header in the form of a ByteString or nothing in case of failure
getHeaderStr msg =
    let 
        head = getHeaderStrUnsafe msg
    in 
        if head == msg then Nothing
        else Just head

-- | Same as getHeaderStr but does not check if the element is present
getHeaderStrUnsafe :: S.ByteString
                   -> S.ByteString
getHeaderStrUnsafe msg = fst3 (msg =~ "\n\n" :: (S.ByteString, S.ByteString, S.ByteString))

-- | Retrieve the header as ByteString from an article in the form of a ByteString,
-- may fail if the format of the article is not coforming to the standard
getBodyStr :: S.ByteString          -- ^ the article in the form of a ByteString
           -> Maybe S.ByteString    -- ^ just the retrieved header in the form of a ByteString or nothing in case of failure
getBodyStr msg =
    let
        body = getBodyStrUnsafe msg
    in
        if body == (C.pack "") then Nothing
        else Just body

-- | Same as getBodyStr but does not check if the element is present
getBodyStrUnsafe :: S.ByteString
                 -> S.ByteString
getBodyStrUnsafe msg = thr3 (msg =~ "\n\n" :: (S.ByteString, S.ByteString, S.ByteString))

-- TODO
unfoldHeader :: String -> String
unfoldHeader = id

-- | Retrieve the labes of the fields in the header of an article
getFieldLabels :: S.ByteString  -- ^ the header in the form of a ByteString 
               -> [String]      -- ^ a list of found labels in the form of normal Strings (!)
getFieldLabels header = map (strToUpper . (\s -> takeWhile (/= ':') s)) (splitOn "\n" (unfoldHeader $ C.unpack header))

-- | Check whether the passed header has all the required fields (from, subject, newsgroups, <MessageID>)
checkRequiredFields :: S.ByteString -- ^ the header of the an article in the form of a ByteString
                    -> [String]     -- ^ list of required for POST or IHAVE
                    -> Bool         -- ^ information whether the header contains all the required fields or not
checkRequiredFields header reqs = all (`elem` reqs) (getFieldLabels header)

-- | List of all required header fields for a POST command
requiredForPOST :: [String]
requiredForPOST = ["FROM", "SUBJECT", "NEWSGROUPS"]

-- | List of all required header fields for IHAVE command
requiredForIHAVE :: [String]
requiredForIHAVE = ["FROM", "SUBJECT", "NEWSGROUPS", "MESSAGE-ID"]

-- | Transform the header in the form of a ByteString into a list of tuples (label, value)
headerToTuples :: S.ByteString                      -- ^ the header in the form a raw ByteString 
               -> [(S.ByteString, S.ByteString)]    -- ^ the list of tuples of labels and values
headerToTuples header = 
    let
        fields = map (\s -> C.pack s) (splitOn "\n" (unfoldHeader $ C.unpack header))
    in
        map (\s -> (fst3 (s =~ ": " :: (S.ByteString, S.ByteString, S.ByteString)), thr3 (s =~ ": " :: (S.ByteString, S.ByteString, S.ByteString)))) fields

-- | Find a particular header field in the header of an article and return it as a tuple (label, value)
getFieldTuple :: S.ByteString                       -- ^ the header of an article
              -> String                             -- ^ the name of the field (label)
              -> Maybe (S.ByteString, S.ByteString) -- ^ just the pair representing the header or nothing in case of failure
getFieldTuple header field = 
    return (map (\(y1,y2) -> ((strToUpper . C.unpack) y1, y2)) (headerToTuples header)) >>= 
    \y -> L.find (\w -> fst w == (strToUpper field)) y >>=
    \(z1,z2) -> return (C.pack z1, z2)

-- | Parse the list of newsgroups into a list
newsgroupsToList :: (S.ByteString, S.ByteString)    -- ^ a tuple of form ("newsgroups", ng1, ng2, ..., ngn)
                 -> Maybe [String]                  -- ^ just the list of newsgroups or nothing in case of failure
newsgroupsToList (label, value) = if (T.unpack $ T.strip $ T.pack $ strToUpper $ C.unpack $ label) /= "NEWSGROUPS" 
                                    then Nothing
                                  else Just $ map (T.unpack . T.strip . T.pack) $ splitOn "," $ C.unpack value

-- | Retrieves the messageID of an artice in the form of a ByteString
getMessageID :: S.ByteString    -- ^ literal contets of an article
             -> Maybe String    -- ^ found messageID without brackets
getMessageID cont = getHeaderStr cont >>= 
    \x -> return (map (\(y1,y2) -> ((strToUpper . C.unpack) y1, y2)) (headerToTuples x)) >>= 
    \y -> L.find (\w -> fst w == "MESSAGE-ID") y >>= 
    \z -> return ((C.unpack . S.tail . S.init) $ snd z)

-- | Retrieve the list of newsgroups from a given article in the form of a ByteString
getNewsgroups :: S.ByteString       -- ^ literal contents of an article
              -> Maybe [String]     -- ^ just the list of newsgroups found or nothing in case of failure
getNewsgroups cont = getHeaderStr cont >>=
    \x -> getFieldTuple x "Newsgroups" >>=
    \y -> newsgroupsToList y

-- -----------------------------------------------------------------------------
-- Helper fucntions (not exported by the module so not visile in the documentation either)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thr3 :: (a,b,c) -> c
thr3 (_,_,x) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

strToUpper :: [Char] -> [Char]
strToUpper str = map toUpper str
