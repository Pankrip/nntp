{-# LANGUAGE OverloadedStrings #-}
module Init.InternalSpec where

import Data.ByteString.Lazy
import Test.Hspec
import Init.Internal
import Options

isLeft r = case r of
		Left _ -> True
		_ -> False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
	describe "Init" $ do
		describe "parseArgs" $ do
			it "parses command line options (short)" $ do
				parseArgs ["-c", "config.yaml"]
				`shouldBe`
				Right (Options { confFile = Just ("config.yaml" :: ByteString) })
			it "parses command line options (long)" $ do
				parseArgs ["--port", "8080"]
				`shouldBe`
				Right (Options { port = Just ("8080" :: ByteString) })
			it "returns set of default Options when there's no arguments" $ do
				parseArgs []
				`shouldBe`
				Right (defaultOptions)
			it "should fail when there are non option arguments" $ do
				parseArgs ["trailing arg"]
				`shouldSatisfy`
				isLeft
			it "should fail when there's option without argument" $ do
				parseArgs ["-h"]
				`shouldSatisfy`
				isLeft

