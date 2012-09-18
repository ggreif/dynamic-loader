import System.Directory
import System.IO
import Data.List
import Control.Monad
import Network

import DynamicLoader.DynamicLoader

main 
    = do cwd <- getCurrentDirectory 

         basep <- loadPackage "base" (Just ppath) Nothing Nothing
         h98p  <- loadPackage "haskell98" (Just ppath) Nothing Nothing

         (mods, plugins) <- loadPlugins cwd

         socket <- listenOn (PortNumber 8080)
         server cwd plugins socket

         unloadPlugins mods

         unloadPackage basep
         unloadPackage h98p

    where ppath = "/usr/lib/ghc-6.2/"

server cwd plugins socket 
    = do (handle, name, _) <- accept socket
         req <- hGetLine handle

         let mfile = parseRequest cwd req

         maybe (errorPage handle) (responsePage plugins handle) mfile

         hClose handle

         server cwd plugins socket


loadPlugins cwd
    = do files <- getDirectoryContents ppath
         let plugs = filter (\a -> "plugin" `isSuffixOf` a) files
             names = map (\s -> reverse $ drop 7 (reverse s)) plugs

         lms <- mapM (\n -> loadModule n (Just ppath) (Just "plugin")) names

         resolveFunctions

         initfuncs <- mapM (\lm -> loadFunction lm "plugin") lms

         plugins <- mapM return 
                         (initfuncs :: [(String, String -> IO String)])

         return (lms, plugins)

    where ppath = cwd ++ "/plugins"

unloadPlugins plugins
    = do mapM_ unloadModule plugins

parseRequest cwd str 
    = let parts = words str
      in case parts of
                    ["GET", name, "HTTP/1.1"] -> Just (cwd ++ name)
                    _                         -> Nothing

responsePage plugins handle file
    = do let mpfunc = find (\(suff, func) -> isSuffixOf suff file) plugins
         str <- catch (readFile file) (\_ -> return "")
         hPutStrLn handle "HTTP/1.1 200 Ok\n"
         case mpfunc of
                     Just (_, f) -> do str' <- f str
                                       hPutStr handle str'
                     Nothing     -> hPutStr handle str

errorPage handle
    = do hPutStrLn handle "HTTP/1.1 400 Bad Request\n"
         hPutStr handle "<html><body>400 Bad Request</body></html>"
