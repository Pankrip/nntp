{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( article
    , group
    , body
    , head
    , stat
    , list
	) where

import Prelude hiding (head)
import qualified Prelude as P (head)
import qualified Client.Descriptor as CD
import qualified Network.Socket.ByteString as N
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified State as S
import qualified Group as G
import qualified Article as A
import qualified Storage as ST
import qualified Text.Read as R
import Data.Char (toUpper)
import Responses

-- -----------------------------------------------------------------------------
-- Functions responsible for exectuing particular commands

-- | Execute the ARTICLE command
article :: CD.ClientDescriptor      -- ^ current state of the client
        -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
        -> IO CD.ClientDescriptor   -- ^ new state of the client
article cd args = 
    case null args of
        True -> 
            case (CD.group cd, CD.article cd) of
                (Just grp, Just art) ->
                    case G.getNumericID art grp of
                        Nothing -> 
                            N.sendAll (CD.socket cd) articleNotInGrpResponse >>
                            return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                        Just numID -> 
                            N.sendAll (CD.socket cd) (articleRetrievedResponse (show numID) ("<" ++ (A.identifier art) ++ ">")) >>
                            S.getStorageLocation >>= \x -> A.getContent art x >>= \y -> N.sendAll (CD.socket cd) y >>
                            return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                (_,_) -> 
                    N.sendAll (CD.socket cd) articlePointerResponse >>
                    return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
        False ->
            let 
                art = C.unpack $ P.head args
            in
                if P.head art == '<' && last art == '>' then
                    let 
                        trimmed = (tail . init) art
                    in
                        S.getStorage >>= \s -> case ST.findArticleByMsgID s trimmed of
                            Nothing ->
                                N.sendAll (CD.socket cd) articleNotFoundResponse >>
                                return  (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                            Just foundArticle ->
                                N.sendAll (CD.socket cd) (articleRetrievedNoNumIDResponse art) >>
                                S.getStorageLocation >>= \x -> A.getContent foundArticle x >>= \y ->
                                return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                else
                    case R.readMaybe art :: Maybe Int of
                        Nothing ->
                            N.sendAll (CD.socket cd) commandSyntaxErrorResponse >>
                            return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                        Just numericID ->
                            case (CD.group cd) of
                                Nothing -> 
                                    N.sendAll (CD.socket cd) serverDataValidityErrResponse >>
                                    return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                                Just validGrp ->
                                    case G.getArtNumID validGrp numericID of
                                        Nothing -> 
                                            N.sendAll (CD.socket cd) articleNotInGrpResponse >>
                                            return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString)})
                                        Just foundArticle ->
                                            N.sendAll (CD.socket cd) (articleRetrievedResponse (show numericID) ("<" ++ (A.identifier foundArticle) ++ ">")) >>
                                            S.getStorageLocation >>= \x -> A.getContent foundArticle x >>= \y -> N.sendAll (CD.socket cd) y >>
                                            return (cd { CD.lastCmd = Just ("ARTICLE" :: L.ByteString), CD.article = Just foundArticle})

-- the BODY as well as HEAD and STAT functions will be almost identical, so we shall we leave them for now

-- | Execute the BODY command
body :: CD.ClientDescriptor      -- ^ current state of the client
     -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
     -> IO CD.ClientDescriptor   -- ^ new state of the client
body cd args = undefined

-- | Execute the HEAD command
head :: CD.ClientDescriptor      -- ^ current state of the client
      -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
      -> IO CD.ClientDescriptor   -- ^ new state of the client
head cd args = undefined

-- | Execute the STAT command
stat :: CD.ClientDescriptor      -- ^ current state of the client
     -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
     -> IO CD.ClientDescriptor   -- ^ new state of the client
stat cd args = undefined

-- | Execute the GROUP command
group :: CD.ClientDescriptor      -- ^ current state of the client
      -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
      -> IO CD.ClientDescriptor   -- ^ new state of the client
group cd args =
    case null args of
        True ->
            N.sendAll (CD.socket cd) commandSyntaxErrorResponse >>
            return (cd { CD.lastCmd = Just ("GROUP" :: L.ByteString)})
        False ->
            let 
                grp = (strToUpper . C.unpack) $ P.head args
            in
                S.getStorage >>= \s -> case ST.getGroup s grp of
                    Nothing ->
                        N.sendAll (CD.socket cd) groupNotFoundErrResponse >>
                        return (cd { CD.lastCmd = Just ("GROUP" :: L.ByteString)})
                    Just foundGroup -> 
                        case thr3 (G.getNumericsForGrp foundGroup) of
                            0 ->
                                N.sendAll (CD.socket cd) (groupSelectedResponse (thr3 fg) (fst3 fg) (snd3 fg) (G.name foundGroup)) >>
                                return (cd { CD.lastCmd = Just ("GROUP" :: L.ByteString), CD.group = Just foundGroup, CD.article = undefined})
                                    where fg = G.getNumericsForGrp foundGroup
                            _ ->
                                N.sendAll (CD.socket cd) (groupSelectedResponse (thr3 fg) (fst3 fg) (snd3 fg) (G.name foundGroup)) >>
                                return (cd { CD.lastCmd = Just ("GROUP" :: L.ByteString), CD.group = Just foundGroup, CD.article = Just ((G.articles foundGroup) !! 0)})
                                    where fg = G.getNumericsForGrp foundGroup

-- | Execute the LIST command (needs to be looked into because of no current support for prohibition of posting)
list  :: CD.ClientDescriptor      -- ^ current state of the client
      -> [S.ByteString]           -- ^ tokenised list of arguments passed by the client with the command
      -> IO CD.ClientDescriptor   -- ^ new state of the client
list cd args =
    S.getStorage >>= \s -> N.sendAll (CD.socket cd) listResponse >>
    listHelper cd s

-- Helper function for the list command
listHelper :: CD.ClientDescriptor
           -> ST.Storage
           -> IO CD.ClientDescriptor
listHelper cd (ST.Storage{ST.groups = []}) = 
    N.sendAll (CD.socket cd) (C.pack ".\r\n") >> 
    return (cd { CD.lastCmd = Just ("LIST" :: L.ByteString)})
listHelper cd (ST.Storage{ST.groups = (g:gs)}) = 
    N.sendAll (CD.socket cd) (C.pack ((G.name g) ++ " " ++ (show (fst3 fg)) ++ " " ++ (show (snd3 fg)) ++ "n" ++ "\r\n")) >>
    listHelper cd (ST.Storage{ST.groups = gs})
        where fg = G.getNumericsForGrp g

-- -----------------------------------------------------------------------------
-- Helper functions

strToUpper :: [Char] -> [Char]
strToUpper str = map toUpper str

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thr3 :: (a,b,c) -> c
thr3 (_,_,x) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x
