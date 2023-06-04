{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import System.Posix.IO.Select (select')
import System.Posix.IO.Select.Types
import System.Posix.Types (Fd(..))
import Network.Socket.ByteString (recv, sendAll)
import Foreign.C.Types (CInt)
import Data.Map as M

type SockMap = M.Map Foreign.C.Types.CInt Socket

main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "3000"
    E.bracket (open addr) close loop
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr

    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

    loop sock = do 
        fd <- fdSocket sock
        loop' (Fd fd) (M.fromList [(fd, sock)])

    loop' :: Fd -> SockMap -> IO ()
    loop' fd dict = do
        res <- select' [fd] [] [] (Time $ CTimeval (fromInteger 1) (fromInteger 0))  
        case res of 
            Nothing -> print ("error happens")
            Just (rs, ws, es) -> deal dict rs
        loop' fd dict

    deal :: SockMap -> [Fd] -> IO ()
    deal dict [] = return ()
    deal dict ((Fd x):xs) = do 
        let sock = M.lookup x dict
        case sock of 
          Nothing -> print ("no")
          Just sock -> do
            msg <- recv sock 1024
            putStr "Received: "
            C.putStrLn msg
            {-deal dict xs-}
