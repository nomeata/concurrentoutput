import System.Terminal.Concurrent
import Control.Concurrent

testThread writeC n1 n2 = do
	t <- myThreadId
	threadDelay $ n1 * 100000
	writeC("Thread " ++ show t ++ ": ")
	threadDelay $ n1 * 100000
	writeC "still working... "
	writeC ""
	threadDelay $ n2 * 100000
	writeC "done\n"

main = do
	writeC <- getConcurrentOutputter
	mapM forkIO  $ zipWith (testThread writeC) [0,0,3,5,2,3] [4,8,5,4,1,2]
	threadDelay $ 15*100000
	
