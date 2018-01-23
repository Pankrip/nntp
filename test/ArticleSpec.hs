module ArticleSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Maybe
import Article
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Article" $ do
        describe "getHeaderStr" $ do
            it "retrieves the header part of an article" $ do
                getHeaderStr holder1 `shouldBe` Just holder2
            it "fails when the structure of the article is not valid" $ do
                getHeaderStr holder3 `shouldBe` Nothing
            it "works even if a second pair of newlines is encountered in the body" $ do
                getHeaderStr holder4 `shouldBe` Just holder2
            it "should return something with a length less than or equal to the original" $ property $
                (\x -> if getHeaderStr x == Nothing then True else S.length (fromJust (getHeaderStr x)) < S.length x)
        describe "getBodyStr" $ do
            it "retrieves the body part of an article" $ do
                getBodyStr holder1 `shouldBe` (Just $ C.pack "The body is here")
            it "fails when the structure of the article is not valid" $ do
                getBodyStr holder3 `shouldBe` Nothing
            it "works even if a second pair of newlines is encountered in the body" $ do
                getBodyStr holder4 `shouldBe` (Just $ C.pack "The body is here\n\nandhere")
        describe "unfoldHeader" $ do
            it "should split the header using newline" $ do
                pending
        describe "getFieldLabels" $ do
            it "should retrieve the list of labels of all header fields in the form of a list of Strings" $ do
                getFieldLabels holder2 `shouldBe` ["SUBJECT", "FROM", "NEWSGROUPS"]
        describe "checkRequiredFields" $ do
            it "should check if all the required fields in an article (subject, from, newsgroups) have been passed" $ do
                (checkRequiredFields holder2 requiredForPOST) `shouldBe` True
        describe "headerToTuples" $ do
            it "should parse the header into tuples of the form (label, value)" $ do
                headerToTuples holder2 `shouldBe` holder7
        describe "newsgroupsToList" $ do
            it "should transform the appropriate tuple into a list of newsgroups" $ do
                newsgroupsToList holder5 `shouldBe` (Just ["test1", "test2", "test3"])
            it "fails when the label is invalid" $ do
                newsgroupsToList holder6 `shouldBe` Nothing
        describe "getMessageID" $ do
            it "should retrieve the message-id value from an article" $ do
                getMessageID holder8 `shouldBe` Just "1233213"
            it "should fail if there is no Message-ID field in an article" $ do
                getMessageID holder1 `shouldBe` Nothing
        describe "getFieldTuple" $ do
            it "should grab a given header field from a header" $ do
                getFieldTuple holder2 "subject" `shouldBe` Just (C.pack "SUBJECT", C.pack "testsubject")
        describe "getNewsgroups" $ do
            it "should grab the list of newsgroups from an article" $ do
                getNewsgroups holder1 `shouldBe` (Just ["test1", "test2", "test3"])
            it "may fail if during the exection process an invalid argument is encountered" $ do
                getNewsgroups holder9 `shouldBe` Nothing

holder1 :: S.ByteString
holder1 = C.pack "Subject: testsubject\nFrom: emailhere\nNewsgroups: test1,test2,test3\n\nThe body is here"

holder2 :: S.ByteString
holder2 = C.pack "Subject: testsubject\nFrom: emailhere\nNewsgroups: test1,test2,test3"

holder3 :: S.ByteString
holder3 = C.pack "Subject: testsubject\nFrom: emailhere\nNewsgroups: test1,test2,test3\nThe body is here"

holder4 :: S.ByteString
holder4 = C.pack "Subject: testsubject\nFrom: emailhere\nNewsgroups: test1,test2,test3\n\nThe body is here\n\nandhere"

holder5 :: (S.ByteString, S.ByteString)
holder5 = (C.pack "Newsgroups", C.pack "test1,   test2,   test3")

holder6 :: (S.ByteString, S.ByteString)
holder6 = (C.pack "Date", C.pack "test1,   test2,   test3")

holder7 ::[(S.ByteString, S.ByteString)]
holder7 = [(C.pack "Subject", C.pack "testsubject"),(C.pack "From", C.pack "emailhere"),(C.pack "Newsgroups", C.pack "test1,test2,test3")]

holder8 :: S.ByteString
holder8 = C.pack "Subject: testsubject\nMessage-ID: <1233213>\nNewsgroups: test1,test2,test3\n\nThe body is here"

holder9 :: S.ByteString
holder9 = C.pack "Monad is a burrito"