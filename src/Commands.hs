{-# LANGUAGE OverloadedStrings #-}

module Commands
    ( article
    , group
	) where

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
                art = C.unpack $ head args
            in
                if head art == '<' && last art == '>' then
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

-- the BODY as well as HEAD and STAT functions will be almost identical, so we shall we leave it for now

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
                grp = (strToUpper . C.unpack) $ head args
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


-- -----------------------------------------------------------------------------
-- Standard server responses with according response codes

articleRetrievedResponse :: String -> String -> S.ByteString
articleRetrievedResponse n msgid = C.pack $ "220 " ++ n ++ " " ++ msgid ++ " article retrieved - head and body follow\r\n"

articleRetrievedNoMsgIDResponse :: String -> S.ByteString
articleRetrievedNoMsgIDResponse n = C.pack $ "220 " ++ n ++ " article retrieved - head and body follow\r\n"

articleRetrievedNoNumIDResponse :: String -> S.ByteString
articleRetrievedNoNumIDResponse msgid = C.pack $ "220 " ++ msgid ++ " article retrieved - head and body follow\r\n"

articleNotInGrpResponse :: S.ByteString
articleNotInGrpResponse = C.pack "423 no such article number in this group\r\n"

articleNotFoundResponse :: S.ByteString
articleNotFoundResponse = C.pack "430 no such article found\r\n"

articlePointerResponse :: S.ByteString
articlePointerResponse = C.pack "520 article not retrieved because no current aricle pointer has been set\r\n"

commandSyntaxErrorResponse :: S.ByteString
commandSyntaxErrorResponse = C.pack "501 command syntax error\r\n"

serverDataValidityErrResponse :: S.ByteString
serverDataValidityErrResponse = C.pack "503 program fault - command not performed\r\n"

groupNotFoundErrResponse :: S.ByteString
groupNotFoundErrResponse = C.pack "411 no such news group\r\n"

groupSelectedResponse :: Int -> Int -> Int -> String -> S.ByteString
groupSelectedResponse n f l s = C.pack $ "211 " ++ (show n) ++ " " ++ (show f) ++ " " ++ (show l) ++ " " ++ s ++  "group selected\r\n"

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
