{-# LANGUAGE KindSignatures, ConstraintKinds,
             TypeFamilies, MultiParamTypeClasses #-}

module System.Plugins.Criteria.LoadCriterion {-(LoadCriterion(..))-} where

-- The 'Constraint' kind is defined in 'GHC.Exts'

import GHC.Exts
import System.Plugins.DynamicLoader
import Data.Dynamic
import Control.Monad.IO.Class

class LoadCriterion (c :: Constraint) t where
  data Criterion c t
  type Effective c t :: *
  addDynamicLibrary :: Criterion c t -> String -> IO ()
  addDynamicLibrary _ = addDLL
  resolveSymbols :: Criterion c t -> IO ()
  resolveSymbols _ = resolveFunctions
  loadQualified :: c => Criterion c t -> String -> Effective c t


-- Safe criteria follow

-- | When the symbol's type is Typeable we load from the suffixed symbol and
-- | try to resolve it.
instance LoadCriterion (Typeable t) t where
  data Criterion (Typeable t) t = DynamicCriterion
  type Effective (Typeable t) t = IO (Maybe t)
  loadQualified DynamicCriterion name = loadQualifiedDynFunction (adornSymbol name)
    where adornSymbol n = n ++ "Dyn"


loadQualifiedDynFunction :: Typeable t => String -> IO (Maybe t)
loadQualifiedDynFunction name = fmap fromDynamic dyn
  where dyn :: IO Dynamic
        dyn = loadQualifiedFunction name


-- | When the symbol's type is Typeable and we are in a monad that can
-- | reliably fail, we load from the suffixed symbol and try to resolve it,
-- | failing when the type does not correspond with the expectation.
instance LoadCriterion (Typeable t, MonadIO m) t where
  data Criterion (Typeable t, MonadIO m) t = DynamicFailableCriterion
  type Effective (Typeable t, MonadIO m) t = m t
  loadQualified DynamicFailableCriterion name = do sym <- liftIO $ loadQualifiedDynFunction (adornSymbol name)
                                                   case sym of
                                                     Nothing -> liftIO $ fail ("symbol " ++ name ++ " does not have the expected type")
                                                     Just it -> return it
    where adornSymbol n = n ++ "Dyn"

