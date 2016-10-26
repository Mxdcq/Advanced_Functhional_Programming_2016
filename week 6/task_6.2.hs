import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad
import Control.Concurrent
import Text.Printf
import Data.List

a=10
b=20
c=30
d=40

main =do m <- newEmptyMVar
         n <- newEmptyMVar
         forkIO $ putMVar m (a + b)
         forkIO $ putMVar n (c + d)
         r <- takeMVar m
         print r
         r <- takeMVar n
         print r
         print (a+b+c+d)
