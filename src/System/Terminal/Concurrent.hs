-----------------------------------------------------------------------------
-- |
-- Module      :  System.Terminal.Concurrent
-- Copyright   :  © 2009 Joachim Breitner 
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  Joachim Breitner <mail@joachin-breitner.de>
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- This library provides a simple interface to output status
-- messages from more than one thread.
--
-- It will continue adding information (such as dots, or "done")
-- to the correct line corresponding to the issuing thread and continue
-- scrolling when a line is done.
--
-----------------------------------------------------------------------------
module System.Terminal.Concurrent 
	( getConcurrentOutputter
	) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import System.IO

-- What threadid has written to what line, counting from below
type OutputState = [(ThreadId,String)]

-- | Returns an IO action to be called to output strings in a thread-safe manner.
getConcurrentOutputter :: IO (String -> IO ())
getConcurrentOutputter = do
	chan <- newChan
	isTerm <- hIsTerminalDevice stdout
	osRef <- newIORef []	
	lock <- newQSem 1
	return $ \s -> unless (null s) $ do
		waitQSem lock
		t <- myThreadId
		os <- readIORef osRef

	        let all_lines = lines $ maybe s (++s) (lookup t os)
		let done = last s == '\n'

		let (done_lines, current_line) =
			if done || null all_lines
                        then (all_lines,     Nothing)
			else (init all_lines, Just (last all_lines)) 

		when isTerm $ do
			unless (null os) $ do
				-- Go up some lines
				putStr $ "\ESC[" ++ show (length os) ++ "A"
				-- Clear everything
				unless (null os) $ putStr "\ESC[0J"
			-- Go up once more, to make it fit
			putStr $ "\ESC[1A"

			-- Write a new done line, if suitable
			mapM_ putLnStr done_lines
		unless isTerm $ do
			mapM_ putStrLn done_lines

		-- Update line
		let os'= replaceAppendOrDelete t current_line os

		when isTerm $ do
			-- Write the new not-yet-done lines
			mapM_ (putLnStr) $ map snd os'

			-- Go to the beginning of the next new line
			putStrLn ""

		writeIORef osRef os'
		signalQSem lock

replaceAppendOrDelete :: (Eq a) => a -> Maybe b -> [(a,b)] -> [(a,b)]
replaceAppendOrDelete a Nothing  []                     = []
replaceAppendOrDelete a (Just b) []                     = [(a,b)]
replaceAppendOrDelete a Nothing  ((a',_):xs)  | a == a' = xs
replaceAppendOrDelete a (Just b) ((a',b'):xs) | a == a' = (a,b):xs
replaceAppendOrDelete a mbB      (x:xs)                 = x : replaceAppendOrDelete a mbB xs

putLnStr s = putStr ('\n':s)
