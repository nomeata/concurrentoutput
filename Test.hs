import System.Terminal.Concurrent
import Control.Exception
import Prelude hiding (catch)
import Control.Concurrent
import Control.Monad

testThread writeC n1 n2 crash = do
	t <- myThreadId
	threadDelay $ n1 * 100000
	writeC("Thread " ++ show t ++ ": ")
	threadDelay $ n1 * 100000
	writeC "still working... "
        when crash $
            error "Thread crashed"
	writeC ""
	threadDelay $ n2 * 100000
	writeC "done\n"
        return True

main = do
	writeC <- getConcurrentOutputter
	forkSequence $ zipWith3 (testThread writeC) [0,0,3,5,2,3] [4,8,5,4,1,2] (False:True:repeat False)

-- Enable for parallel downloads
forkSequence acts = do
    sem <- newQSem 5
    mvars <- forM acts $ \act -> do
        mvar <- newEmptyMVar
        forkIO $ do 
            waitQSem sem
            catch (act >>= putMVar mvar) (\e -> putMVar mvar (throw e))
            signalQSem sem
        return mvar
    mapM takeMVar mvars
	
