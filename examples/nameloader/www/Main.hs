import System.IO
import Data.List
import qualified Data.HashTable as HT
import Control.Monad
import Control.Concurrent
import Network

import DynamicLoader.NameLoader


main 
    = do moduleh <- HT.new (==) HT.hashString 

         setEnvironment Nothing Nothing (Just ppath) Nothing Nothing

         basep <- loadModule "base"
         h98p  <- loadModule "haskell98"

         socket <- listenOn (PortNumber 8080)

         server moduleh socket

    where ppath = "/usr/lib/ghc-6.2/"

server moduleh socket 
    = do (handle, name, _) <- accept socket
         forkIO (client moduleh handle)
         server moduleh socket

client moduleh handle
    = do req <- hGetLine handle

         let mfile = parseRequest req

         maybe (errorPage handle) (responsePage moduleh handle) mfile

         hClose handle

parseRequest str 
    = let parts = words str
      in case parts of
                    ["GET", name, "HTTP/1.1"] -> Just (toDot $ drop 1 name)
                    _                         -> Nothing
    where toDot str = map (\a -> case a of { '/' -> '.'; a -> a }) str

responsePage moduleh handle file
    = do mlm <- HT.lookup moduleh file
         lm <- case mlm of 
                        Just lm -> do reloadModule lm True
                                      return lm
                        Nothing -> do lm <- loadModule file
                                      HT.insert moduleh file lm
                                      return lm

         func <- loadFunction lm "page"
         str  <- func

         hPutStr handle hdr
         hPutStr handle str

    where hdr = "HTTP/1.1 200 Ok\n\n"

errorPage handle
    = do hPutStrLn handle "HTTP/1.1 400 Bad Request\n"
         hPutStr handle "<html><body>400 Bad Request</body></html>"
