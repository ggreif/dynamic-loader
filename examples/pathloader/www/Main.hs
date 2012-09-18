import System.IO
import System.Directory
import Data.List
import qualified Data.HashTable as HT
import Control.Monad
import Control.Concurrent
import Network

import PathLoader


main 
    = do basep <- loadModule "/usr/lib/ghc-6.2/HSbase.o" MT_Package
         h98p  <- loadModule "/usr/lib/ghc-6.2/HShaskell98.o" MT_Package

         socket <- listenOn (PortNumber 8080)

         server socket

server socket 
    = do (handle, name, _) <- accept socket
         forkIO (client handle)
         server socket

client handle
    = do req <- hGetLine handle

         let mfile = parseRequest req

         maybe (errorPage handle) (responsePage handle) mfile

         hClose handle

parseRequest str 
    = let parts = words str
      in case parts of
                    ["GET", name, "HTTP/1.1"] -> Just (drop 1 name)
                    _                         -> Nothing

responsePage handle file
    = do cwd <- getCurrentDirectory
         lm <- loadModule (cwd ++ "/" ++ file) MT_Module
         
         func <- loadFunction lm "page"
         str  <- func

         unloadModule lm

         hPutStr handle hdr
         hPutStr handle str

    where hdr = "HTTP/1.1 200 Ok\n\n"

errorPage handle
    = do hPutStrLn handle "HTTP/1.1 400 Bad Request\n"
         hPutStr handle "<html><body>400 Bad Request</body></html>"
