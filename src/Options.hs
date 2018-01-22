{-# LANGUAGE OverloadedStrings #-}

module Options
	( Options (..)
	, options
	, defaultOptions
	, emptyOptions
	) where

import Data.ByteString.Lazy
import Data.ByteString.Builder
import System.Console.GetOpt

-- | Structure describing various configurational aspects of the server
data Options = Options
	{ confFile  :: Maybe ByteString
	, port      :: Maybe ByteString
	, host      :: Maybe ByteString
	, accessLog :: Maybe ByteString
	, errorLog  :: Maybe ByteString
	, directory :: Maybe ByteString
	} deriving (Show, Eq)

options :: [OptDescr (Options -> Options)]
options = [
	  Option ['c'] ["conf"]
		(ReqArg (\c o -> o { confFile = Just (toLazyByteString $ string7 c) }) "FILE")
		"location of server configuration file"
	, Option ['p'] ["port"]
		(ReqArg (\p o -> o { port = Just (toLazyByteString $ string7 p) }) "PORT")
		"port to bind to"
	, Option ['h'] ["name"]
		(ReqArg (\n o -> o { host = Just (toLazyByteString $ string7 n) }) "(IP|Domain)")
		"hostname to listen on"
	, Option [] ["access-log"]
		(ReqArg (\l o -> o { accessLog = Just (toLazyByteString $ string7 l) }) "FILE")
		"location of access log"
	, Option [] ["error-log"]
		(ReqArg (\l o -> o { errorLog = Just (toLazyByteString $ string7 l) }) "FILE")
		"location of error log"
	, Option ['d'] ["dir"]
		(ReqArg (\d o -> o { directory = Just (toLazyByteString $ string7 d) }) "DIR")
		"directory where newsgroups are stored"
	]

defaultOptions :: Options
defaultOptions = Options
	{ confFile  = Just ("config.yaml" :: ByteString)
	, port      = Just ("9111" :: ByteString)
	, host      = Nothing
	, accessLog = Just ("access.log" :: ByteString)
	, errorLog  = Just ("error.log" :: ByteString)
	, directory = Just ("groups" :: ByteString)
	}

emptyOptions :: Options
emptyOptions = Options
	{ confFile  = Nothing
	, port      = Nothing
	, host      = Nothing
	, accessLog = Nothing
	, errorLog  = Nothing
	, directory = Nothing
	}
