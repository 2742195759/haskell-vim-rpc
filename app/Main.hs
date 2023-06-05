{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SelectLib (startSelect
                , SelectEnv(..)
                , Handle(..)
                , echoHandle
                , selectInsert
                )
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import System.Posix.IO.Select (select')
import System.Posix.IO.Select.Types
import System.Posix.Types (Fd(..))
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad.Trans.State.Lazy (StateT, modify, runStateT, get)
import Foreign.C.Types (CInt)
import Control.Monad.Trans.Class (lift)
import Data.Map as M
import Network.Stream (Stream(..))


listenHandle :: Handle
listenHandle sock = do
    (conn, _) <- lift $ accept sock
    oneline <- lift . readLine $ sock
    if oneline == C.pack "xiongkun\n" then 
      lift . print $ "Receive xiongkun."
    else E.throw $ AssertionFailed "not xiongkun"
    (SelectEnv _) <- get
    selectInsert conn echoHandle


main :: IO ()
main = startSelect listenHandle

