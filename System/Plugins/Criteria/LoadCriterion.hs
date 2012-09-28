{-# LANGUAGE KindSignatures, ConstraintKinds,
             TypeFamilies, MultiParamTypeClasses #-}

module System.Plugins.Criteria.LoadCriterion (LoadCriterion(..)) where

-- The 'Constraint' kind is defined in 'GHC.Exts'

import GHC.Exts
import System.Plugins.DynamicLoader
import Data.Dynamic

class LoadCriterion (c :: Constraint) t where
  data Criterion c t
  type Effective c t :: *
  addDynamicLibrary :: Criterion c t -> String -> IO ()
  addDynamicLibrary _ = addDLL
  resolveSymbols :: Criterion c t -> IO ()
  resolveSymbols _ = resolveFunctions
  loadQualified :: c => Criterion c t -> String -> IO (Effective c t)


-- Safe criteria follow

instance LoadCriterion (Typeable t) t where
  data Criterion (Typeable t) t = DynamicCriterion
  type Effective (Typeable t) t = Maybe t
  loadQualified DynamicCriterion name = loadQualifiedDynFunction (adornSymbol name)
    where adornSymbol n = n ++ "Dyn"


loadQualifiedDynFunction :: Typeable t => String -> IO (Maybe t)
loadQualifiedDynFunction name = fmap fromDynamic dyn
  where dyn :: IO Dynamic
        dyn = loadQualifiedFunction name
