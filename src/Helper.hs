
module Helper where


import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Monad


threadPool :: Int -> (a -> b) -> IO(Chan a, Chan b)
threadPool n f = do
  input <- newChan
  output <- newChan
  forM_ [1..n] $ \_ ->
      forkIO $ forever $ do
         i <- readChan input
         let o = f i
         writeChan output o
  return (input, output)


threadPoolIO :: Int -> (a -> IO b) -> IO(Chan a, Chan b)
threadPoolIO n f = do
  input <- newChan
  output <- newChan
  forM_ [1..n] $ \_ ->
      forkIO $ forever $ do
         i <- readChan input
         o <- f i
         writeChan output o
  return (input, output)
