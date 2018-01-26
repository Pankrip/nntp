{-# LANGUAGE OverloadedStrings #-}

module Commands
	( article
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

