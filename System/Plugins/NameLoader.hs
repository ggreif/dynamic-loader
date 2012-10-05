----------------------------------------------------------------------------
-- |
-- Module      :  NameLoader
-- Copyright   :  (c) Hampus Ram 2004, Gabor Greif 2012
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  ggreif+dynamic@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (ghc >= 7.6 only)
--
-- A module that implements dynamic loading.
-- Has smart handling of dependencies and
-- is thread safe.
--
----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module System.Plugins.NameLoader (Module, LoadedModule,
                                  ModuleType(..),
                                  setEnvironment,
                                  addDependency,
                                  delDependency,
                                  delAllDeps,
                                  withDependencies,
                                  loadModule,
                                  unloadModule,
                                  unloadModuleQuiet,
                                  loadFunction,
                                  moduleLoadedAt,
                                  loadedModules,
                                  sm_path,
                                  DL.addDLL) where

import Data.Char (isUpper)

import Control.Concurrent.MVar
import Data.List
import qualified Data.HashTable.IO as HT
import Data.Hashable
import Data.IORef
import System.IO.Unsafe
import System.Directory
import Data.Time
import Control.Exception (catch, SomeException)
import System.Plugins.Criteria.LoadCriterion
import System.Plugins.Criteria.UnsafeCriterion

import qualified System.Plugins.DynamicLoader as DL

type Module = String

newtype LoadedModule = LM Module

data ModuleType = MT_Module
                | MT_Package
                  deriving (Eq, Ord, Show)

type ModuleWT = (String, ModuleType)

type NameDynamics = Either DL.DynamicModule DL.DynamicPackage

type NameDep = [Module]

-- SM reference_count type time module
data NameModule = SM { sm_refc   :: !Int,
                       sm_time   :: UTCTime,
                       sm_deps   :: NameDep,
                       sm_module :: NameDynamics }

-- module_path modudle_suff 
-- pkg_path pkg_prefix pkg_suffix 
-- dependency_map modules
type NameEnvData = (Maybe FilePath, Maybe String,
                    Maybe FilePath, Maybe String, Maybe String,
                    HT.BasicHashTable String [Module],
                    HT.BasicHashTable String NameModule)

{- 

   New SmarEnv that uses bot an IORef and a MVar
   to make it possible to have non blocking functions
   that inspect the state.

   Could perhaps change it to only use IORef (with atomicModifyIORef)
   but lets play safe and have an MVar too.

-}
type NameEnv = (MVar (), IORef NameEnvData)

withNameEnv :: (LoadCriterion c t, Effective c t ~ IO t') => Criterion c t -> NameEnv -> (NameEnvData -> Effective c t) -> Effective c t
withNameEnv crit (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f' crit)
  where f' crit = f

withNameEnvNB :: NameEnv -> (NameEnvData -> IO b) -> IO b
withNameEnvNB (_, ioref) f = readIORef ioref >>= f

modifyNameEnv_ :: NameEnv -> (NameEnvData -> IO NameEnvData) -> IO ()
modifyNameEnv_ (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f >>= writeIORef ioref)

{-# NOINLINE env #-}
env :: NameEnv
env = unsafePerformIO (do modh <- HT.new
                          deph <- HT.new
                          mvar <- newMVar ()
                          ioref <- newIORef (Nothing, Nothing, Nothing,
                                             Nothing, Nothing, deph, modh)
                          return (mvar, ioref))

{-|

Set the environment in wich all module loading will reside. If this
function isn't called the defaults will be used.

The parameters are: Path to modules, module suffix, path to packages,
package prefix and package suffix. The paths will default to current
directory and the rest (in order) to /o/, /HS/ and /o/.

-}
setEnvironment :: Maybe FilePath -> Maybe String -> 
                  Maybe FilePath -> Maybe String -> Maybe String -> IO ()
setEnvironment mpath msuff ppath ppre psuff
    =  modifyNameEnv_ env (\(_, _, _, _, _, deph, modh) ->
                            return (mpath, msuff, ppath, ppre, psuff,
                                    deph, modh))


{-|

Add a module dependency. Any dependencies must be added /before/ any
calls to loadModule or symbols will not be resolved with a crash as
result.

-}
addDependency :: Module -> Module -> IO ()
addDependency from to = withNameEnv UnsafeCriterion env (addDependency' from to)

addDependency' :: Module -> Module -> NameEnvData -> IO ()
addDependency' from to (_, _, _, _, _, deph, _)
    = insertHT_C union deph from [to]

{-|

Delete a module dependency.

-}
delDependency :: Module -> Module -> IO ()
delDependency from to = withNameEnv UnsafeCriterion env (delDependency' from to)

delDependency' :: Module -> Module -> NameEnvData -> IO ()
delDependency' from to (_, _, _, _, _, deph, _)
    = modifyHT (\\[to]) deph from

{-|

Delete all dependencies for a module.

-}

delAllDeps :: Module -> IO ()
delAllDeps from = withNameEnv UnsafeCriterion env (delAllDeps' from)

delAllDeps' :: Module -> NameEnvData -> IO ()
delAllDeps' from (_, _, _, _, _, deph, _)
    = deleteHT deph from

{-|

Do something with the current dependencies of a module. You can't use
(blocking) functions from this module in the function given to
withDependencies. If you do so, a deadlock will occur.

-}
withDependencies :: (LoadCriterion c t, Effective c t ~ IO t') => Criterion c t -> Module -> (Maybe [Module] -> Effective c t) -> Effective c t
withDependencies crit from f
    = withNameEnv crit env (\(_,_,_,_,_,deph,_) -> lookupHT deph from >>= f' crit)
  where f' crit = f

{-|

Load a module (or package) and modules it depends on. It is possible
to load a module many times without any error occuring. However to
unload a module one needs to call @unloadModule@ the same number of
times.

Before loading any modules you should add wich dependencies it has
with addDependency (and which dependencies the modules upon which it
depends have).

If the module already has been loaded nothing will be done except
updating the reference count. I.e. if dependencies have been updated
they will be ignored until the module has been completely unloaded and
loaded again.

It treats names begining with uppercase letters (such as @Foo.Bar@) as
modules and other names (such as @base@) as packages.

If any error occurs an exception is thrown.

-}
loadModule :: Module -> IO LoadedModule
loadModule m 
    = do withNameEnv UnsafeCriterion env (\env -> do loadModuleWithDep m env
                                                     DL.resolveFunctions
                                                     return (LM m))

loadModuleWithDep :: Module -> NameEnvData -> IO ()
loadModuleWithDep name
                  env@(_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh name
         (sm, depmods) <- midLoadModule msm name env

         insertHT modh name sm

         mapM_ (\modwt -> loadModuleWithDep modwt env) depmods

midLoadModule :: Maybe NameModule -> Module -> NameEnvData ->
                 IO (NameModule, NameDep)
midLoadModule (Just sm) _ _ = return $ (sm { sm_refc = sm_refc sm + 1 },
                                        sm_deps sm)
midLoadModule Nothing name env@(_, _, _, _, _, deph, _)
    = do (sd, time) <- lowLoadModule (nameToMWT name) env
         depmods <- lookupDefHT deph [] name
         return (SM 1 time depmods sd, depmods)

lowLoadModule :: ModuleWT -> NameEnvData -> IO (NameDynamics, UTCTime)
lowLoadModule (name, MT_Package) (_, _, ppath, ppre, psuff, _, _)
    = do lp <- DL.loadPackage name ppath ppre psuff
         time <- getModificationTime (DL.dp_path lp)
         return (Right lp, time)
lowLoadModule (name, MT_Module) (mpath, msuff, _, _, _, _, _)
    = do lm <- DL.loadModule name mpath msuff
         time <- getModificationTime (DL.dm_path lm)
         return (Left lm, time)

{-|

Unload a module and all modules it depends on. This unloading only
occurs if the module isn't needed by any other libraries or hasn't
been loaded more than once. An exception is thrown in case of error.

-}
unloadModule :: LoadedModule -> IO ()
unloadModule (LM name) 
    = withNameEnv UnsafeCriterion env (unloadModuleWithDep name)

{-|

Same as @unloadModule@ just doesn't trow any exceptions on error.

-}
unloadModuleQuiet :: LoadedModule -> IO ()
unloadModuleQuiet (LM name)
    = withNameEnv UnsafeCriterion env (\env -> catch (unloadModuleWithDep name env)
                                       (\(_ :: SomeException) -> return ()))

unloadModuleWithDep :: Module -> NameEnvData -> IO ()
unloadModuleWithDep name env@(_, _, _, _, _, _, modh)
    = do msm <- lookupHT modh name
         sm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return msm
         
         if sm_refc sm > 1
            then do insertHT modh name (sm { sm_refc = sm_refc sm - 1 })
            else do lowUnloadModule (sm_module sm)
                    deleteHT modh name
 
         mapM_ (\m -> unloadModuleWithDep m env) (sm_deps sm)

lowUnloadModule :: NameDynamics -> IO ()
lowUnloadModule (Left lm)  = DL.unloadModule lm
lowUnloadModule (Right lp) = DL.unloadPackage lp

{-|

Load a function from a module. It cannot load functions from packages
and will throw an exception if one tries to do so. Also throws if an
error occurs.

It seems (but I'm unsure) like any functions loaded will continue to
be valid even after the module it resides in is unloaded. It will also
still be valid if a new version of that module is loaded (it will thus
still call the old function).

-}
loadFunction :: LoadedModule -> String -> IO a
loadFunction (LM m) name
    = withNameEnv UnsafeCriterion env (loadFunction' (nameToMWT m) name)

loadFunction' :: ModuleWT -> String -> NameEnvData -> IO a
loadFunction' (_, MT_Package) _ _ 
    = fail "Cannot load functions from packages"
loadFunction' (mname, _) fname (_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh mname
         sm <- maybe (fail $ "Module " ++ mname ++ " isn't loaded") 
                     return msm

         let Left dm = sm_module sm
         
         DL.loadFunction dm fname


{-|

Give the modification time for a loded module. Will throw an exception
if the module isn't loaded.

-}
moduleLoadedAt :: LoadedModule -> IO UTCTime
moduleLoadedAt (LM m)
    = withNameEnvNB env (moduleLoadedAt' m)

moduleLoadedAt' :: Module -> NameEnvData -> IO UTCTime
moduleLoadedAt' name (_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh name 
         sm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return msm
         return (sm_time sm)

loadedModules :: IO [String]
loadedModules = withNameEnvNB env loadedModules' 

loadedModules' :: NameEnvData -> IO [String]
loadedModules' (_, _, _, _, _, _, modh) 
    = HT.toList modh >>= (\lst -> return (map fst lst))

-- Some helper functions 

sm_path :: NameModule -> FilePath
sm_path sm = case sm_module sm of
                               Left  dm -> DL.dm_path dm
                               Right dp -> DL.dp_path dp

nameToMWT :: String -> ModuleWT
nameToMWT (c:cs) 
    | isUpper c = (c:cs, MT_Module)
    | otherwise = (c:cs, MT_Package)
nameToMWT _ = error "empty module names not allowed"

-- functions to handle HashTables in a better way

-- it seems like it doesn't replace the old value on insert
insertHT :: (Eq key, Hashable key) => HT.BasicHashTable key val -> key -> val -> IO ()
insertHT ht key val 
    = do HT.delete ht key
         HT.insert ht key val

insertHT_C :: (Eq key, Hashable key) => (val -> val -> val) -> HT.BasicHashTable key val -> key -> val -> IO ()
insertHT_C func ht key val
    = do mval <- HT.lookup ht key
         case mval of
                   Just val' -> insertHT ht key (func val val')
                   Nothing   -> insertHT ht key val

modifyHT :: (Eq key, Hashable key) => (val -> val) -> HT.BasicHashTable key val -> key -> IO ()
modifyHT func ht key
    = do mval <- HT.lookup ht key
         case mval of
                   Just val -> insertHT ht key (func val)
                   Nothing  -> return ()

lookupHT :: (Eq key, Hashable key) => HT.BasicHashTable key val -> key -> IO (Maybe val)
lookupHT ht key = HT.lookup ht key

deleteHT :: (Eq key, Hashable key) => HT.BasicHashTable key val -> key -> IO ()
deleteHT ht key = HT.delete ht key

lookupDefHT :: (Eq key, Hashable key) => HT.BasicHashTable key b -> b -> key -> IO b
lookupDefHT ht val key
    = do mval <- HT.lookup ht key
         case mval of
                   Just val -> return val
                   Nothing  -> return val
