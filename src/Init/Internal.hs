module Init.Internal
	( parseConf
	, parseArgs
	) where

import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Yaml.Config
import System.Console.GetOpt
--
import Options

-- | Parses configuration file into a 'Options' structure
parseConf :: FilePath -- ^ location of configuration file
	  -> Options -- ^ Options from configuration file (or default ones, if no option in config was found)
parseConf file = undefined

-- | Parse command line options into a 'Options' structure
parseArgs :: [String]
	  -> Either String Options -- ^ Error message or 
parseArgs t = (
	let header = "NNTP server usage: "
	in
	case getOpt RequireOrder options t of
		-- there's at least one element in the non-options list
		([], [], []) -> Right defaultOptions
		(_, x : _, errs) -> Left (trailingArgsMsg ++ concat errs ++ usageInfo header options)
			where trailingArgsMsg = "trailing options found on command line\n"
		(opts, _, []) -> Right (foldl (flip id) emptyOptions opts)
		(_, _, errs) -> Left (concat errs ++ usageInfo header options)
	)


