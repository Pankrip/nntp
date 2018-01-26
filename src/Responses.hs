module Responses
    ( articleRetrievedResponse
    , articleRetrievedResponse
    , articleRetrievedNoNumIDResponse
    , articleNotInGrpResponse
    , articleNotFoundResponse
    , articlePointerResponse
    , commandSyntaxErrorResponse
    , serverDataValidityErrResponse
    , groupNotFoundErrResponse
    , groupSelectedResponse
    , listResponse
    , nextArticleResponse
    , noNewsGroupSelectedResponse
    , noArtSelectedResponse
    , noNextResponse
	) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

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

listResponse :: S.ByteString
listResponse = C.pack $ "215 list of newsgroups follows\r\n"

nextArticleResponse :: String -> String -> S.ByteString
nextArticleResponse n a = C.pack $ n ++ " " ++ a ++ " article retrieved - request text separately\r\n"

noNewsGroupSelectedResponse :: S.ByteString
noNewsGroupSelectedResponse = C.pack "412 no newsgroup selected\r\n"

noArtSelectedResponse :: S.ByteString
noArtSelectedResponse = C.pack "420 no current article has been selected\r\n"

noNextResponse :: S.ByteString
noNextResponse = C.pack "421 no next article in this group\r\n"