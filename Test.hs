import System.Terminal.Concurrent
import Control.Concurrent

testThread co n1 n2 = do
	t <- myThreadId
	threadDelay $ n1 * 100000
	writeConcurrent co ("Thread " ++ show t ++ ": ")
	threadDelay $ n1 * 100000
	writeConcurrent co "still working... "
	threadDelay $ n2 * 100000
	writeConcurrentDone co "done"

main = do
	co <- startConcurrentOutput
	mapM forkIO  $ zipWith (testThread co) [0,0,3,4,3] [4,8,5,5,2]
	threadDelay $ 15*100000
	
