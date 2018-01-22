module Init
	( getConfig
	) where

import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Yaml.Config
import Network.Socket
import System.Console.GetOpt
--
import Init.Internal
import Options

{-|
 Retrieve server configuration from config file and/or command line
 Arguments are retrieved in following order:
 	1. Command line
	2. Configuration file
	3. Defaults
-} 
getConfig :: [String] -- ^ Command-line options (eg. from 'System.Environment.getArgs')
	  -> Either String Options -- ^ Record with various server configuration parameters or error message
getConfig t = (
	-- TODO: zip with config file
	parseArgs t
	)

