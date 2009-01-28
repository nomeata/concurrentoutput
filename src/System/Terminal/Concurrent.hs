-- | This library provides a simple interface to output status
--   messages from more than one thread.
--
--   It will continue adding information (such as dots, or "done")
--   to the correct line corresponding to the issuing thread and continue
--   scrolling when a line is done.
module System.Terminal.Concurrent 
	( ConcurrentOutput
	, startConcurrentOutput
	, writeConcurrent
	, writeConcurrentDone
	) where

import Control.Concurrent
import Control.Monad
import Data.List
import System.IO

data ConcurrentOutput = ConcurrentOutput { coChan :: Chan (ThreadId, Bool, String) }

-- | Starts the thread responsible for gathering and formatting the outputs.
--    
--   You can not kill this thread, so only start one for your application.
startConcurrentOutput :: IO ConcurrentOutput
startConcurrentOutput = do
	chan <- newChan
	isTerm <- hIsTerminalDevice stdout
	forkIO (receiverThread chan isTerm)
	return (ConcurrentOutput chan)

-- | Begin a new line of output for your thread or, if there already is one, append to it.
--    
--   Do not put newline characters in there, or it will break the output. This
--   will also happen if your total line will be wider than the terminal.
writeConcurrent :: ConcurrentOutput -> String -> IO ()
writeConcurrent (ConcurrentOutput chan) s = do
	threadId <- myThreadId
	writeChan chan (threadId, False, s)

-- | Finish your line of output with the given string.
--    
--   Do not put newline characters in there, or it will break the output. This
--   will also happen if your total line will be wider than the terminal.
writeConcurrentDone :: ConcurrentOutput -> String -> IO ()
writeConcurrentDone (ConcurrentOutput chan) s = do
	threadId <- myThreadId
	writeChan chan (threadId, True, s)

-- What threadid has written to what line, counting from below
type OutputState = [(ThreadId,String)]

receiverThread :: Chan (ThreadId, Bool, String) -> Bool -> IO ()
receiverThread chan True = runner []
	where runner os = do
		(t,d,s) <- readChan chan

	        let s' = maybe s (++s) (lookup t os)
		let os'= if d then filter ((/=t).fst) os
		              else replaceOrAppend t s' os
		unless (null os) $ do
			-- Go up some lines
			putStr $ "\ESC[" ++ show (length os) ++ "A"
			-- Clear everything
			unless (null os) $ putStr "\ESC[0J"
		-- Go up once more, to make it fit
		putStr $ "\ESC[1A"

		-- Write a new done line, if suitable
		when d $ putLnStr s' 

		-- Write the new not-yet-done lines
		mapM_ (putLnStr) $ map snd os'

		-- Go to the beginning of the next new line
		putStrLn ""

		runner os'
-- We do not have a terminal, just output complete lines
receiverThread chan False = runner []
	where runner os = do
		(t,d,s) <- readChan chan

	        let s' = maybe s (++s) (lookup t os)
		let os'= if d then filter ((/=t).fst) os
		              else replaceOrAppend t s' os

		-- Write a new done line, if suitable
		when d $ putStrLn s' 

		runner os'

replaceOrAppend :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
replaceOrAppend a b []                       = [(a,b)]
replaceOrAppend a b ((a',b'):xs) | a == a'   = (a,b):xs
                                 | otherwise = (a',b') : replaceOrAppend a b xs

putLnStr s = putStr ('\n':s)
