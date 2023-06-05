{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SelectLib (startSelect
                , SelectEnv(..)
                , Handle(..)
                , echoHandle
                , selectInsert
                , SelectMonad(..)
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
import Network.StreamSocket


readLine' :: Socket -> IO String
readLine' sock = do 
    line <- readLine sock
    case line of
        Left _ -> E.throw $ E.AssertionFailed "not xiongkun"
        Right s -> return s

vimHandle :: Handle
vimHandle sock = do 
    return ()
    

listenHandle :: Handle
listenHandle sock = do
    (conn, _) <- lift $ accept sock
    oneline <- lift . readLine' $ conn
    case oneline of
      "vim\n" -> do 
        lift . print $ "Receive vim client..., use vimHandle"
        selectInsert conn vimHandle
      _ -> do 
        lift . print $ "Unrecognize connection, use echoHandle."
        selectInsert conn echoHandle

stdinHandle :: Handle
stdinHandle sock = do
    oneline <- lift . readLine' $ sock
    lift . print $ "FromInput: " ++ oneline

initHandle :: SelectMonad ()
initHandle = do
    sock <- lift . mkSocket $ 0
    selectInsert sock stdinHandle


main :: IO ()
main = startSelect initHandle listenHandle
