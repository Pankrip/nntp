{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module StateMachine.Command
	( csm
	, State (START)
	) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Word as W
import qualified System.Posix.Types as T
import qualified "unix-bytestring" System.Posix.IO.ByteString as I
import qualified Control.Concurrent as C
--
import qualified Client.Internal as CL

-- | States of the machine (nodes on the graph)
data State = START -- ^ initial state, gather stage (collect data from socket)
	   | PROCESS -- ^ processing state
	   | NEWTEXT -- ^ "accepting" state, dispatches recieved command and deals with potentially non-empty tail
	   | CHECKEND -- ^ transition state, checks if last byte of last read is == \r
	   | SEQINTRO -- ^ transition state where \r has been recieved at the end of last read but next character is yet unknown
	   deriving (Show, Eq)

-- | convenience aliases
type Buffer = L.ByteString
type Current = S.ByteString

-- | standard network newline
newLine = "\r\n" :: S.ByteString

-- cache those so the cost of running a function is incurred only once
carriageReturn = toWord8 '\r'
lineFeed = toWord8 '\n'

-- | helper function for converting single 'Char' into 'W.Word8'
toWord8 :: Char -> W.Word8
toWord8 c = (toEnum $ (fromEnum c) :: W.Word8)

-- | helper function for removing single \\r\\n sequence from front of the string
stripNewline :: S.ByteString -> S.ByteString
stripNewline s = case S.stripPrefix newLine s of
			Nothing -> s
			Just v -> v

------------------------ Command State Machine -----------------------
-- | Command State Machine
csm :: State -- ^ state the machine will transition to
    -> T.Fd -- ^ file descriptor of client's socket
    -> Buffer -- ^ command buffer
    -> Current -- ^ currently processed string
    -> T.ByteCount -- ^ how many bytes can be read yet (TODO)
    -> CL.ClientDescriptor -- ^ structure describing client connection
--    -> IO ()
    -> IO (CL.ClientDescriptor, Buffer)

-- TODO: make the max argument count
-- csm START fd buf _ max cd | trace "\tcms START\n" False = undefined
csm START fd buf _ max cd = (
	-- (traceIO "\ncsm START\n") >>
	C.threadWaitRead fd >>
	I.fdRead fd (max+1) >>=
	\str -> csm PROCESS fd buf str max cd
	)

csm PROCESS fd buf str max cd = (
	-- (traceIO $ "\n=======\ncsm PROCESS:\n" ++ show buf ++ "\n===\n" ++ show str ++ "\n======\n") >>
	case S.breakSubstring newLine str of
	(h, t) -> (
		if S.null h then
			if S.null t then
				-- (NUL, NUL)
				-- buffer exhausted
				csm START fd buf undefined max cd
			else
				-- (NUL, CRLF rest)
				csm NEWTEXT fd buf (stripNewline t) max cd
		else
			if S.null t then
				-- (str, NUL)
				-- pending
				csm CHECKEND fd (L.append buf (L.fromStrict str)) S.empty max cd
			else
				-- (str, CRLF rest)
				csm NEWTEXT fd (L.append buf (L.fromStrict h)) (stripNewline t) max cd
		)	
	)

csm NEWTEXT fd buf str max cd = (
	-- (traceIO $ "\n=======\ncsm NEWTEXT:\n" ++ show buf ++ "\n===\n" ++ show str ++ "\n======\n") >>
	if L.null buf then 
		-- \r\n\r\n sequence found
		csm PROCESS fd buf str max cd
	else
		CL.dispatchCommand buf cd >>=
		\st -> case st of
			new@(CL.ClientDescriptor { CL.state = CL.QuitMode }) ->
				-- CL.repl CL.QuitMode (L.fromStrict str) new -- terminate connection -- repl QuitMode ...
				return (new, (L.fromStrict str))
			new@(CL.ClientDescriptor { CL.state = CL.PostMode }) ->
				-- CL.repl CL.PostMode (L.fromStrict str) new -- transition -- repl PostMode ...
				return (new, (L.fromStrict str))
			new@(CL.ClientDescriptor { CL.state = CL.CommandMode }) ->
				csm PROCESS fd L.empty str max new
		{-
		case CL.dispatchCommand buf cd of
			new@(CL.ClientDescriptor { CL.state = CL.QuitMode }) -> CL.repl CL.QuitMode (L.fromStrict str) -- terminate connection -- repl QuitMode ...
			new@(CL.ClientDescriptor { CL.state = CL.PostMode }) -> CL.repl CL.PostMode (L.fromStrict str) -- transition -- repl PostMode ...
			new@(CL.ClientDescriptor { CL.state = CL.CommandMode }) -> csm PROCESS fd empty str max new
		-}
	)

csm CHECKEND fd buf str max cd = (
	-- (traceIO $ "\n=======\ncsm CHECKEND:\n" ++ show buf ++ "\n===\n" ++ show str ++ "\n======\n") >>
	if L.last buf == carriageReturn then
		csm SEQINTRO fd buf str max cd
	else
		csm START fd (L.append buf (L.fromStrict str)) undefined max cd
	)

csm SEQINTRO fd buf str max cd = (
	-- (traceIO $ "\n=======\ncsm SEQINTRO:\n" ++ show buf ++ "\n===\n" ++ show str ++ "\n======\n") >>
	C.threadWaitRead fd >>
	I.fdRead fd (1 :: T.ByteCount) >>=
	\cs -> let c = S.head cs
		in
		if c == lineFeed then
			csm NEWTEXT fd (L.append buf (L.fromStrict str)) S.empty max cd
		else
			csm START fd (L.snoc (L.append buf (L.fromStrict str)) c) undefined max cd
	)

------------------------ End Of Command State Machine -----------------------

