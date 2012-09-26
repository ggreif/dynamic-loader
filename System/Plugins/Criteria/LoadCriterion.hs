{-# LANGUAGE KindSignatures, ConstraintKinds,
             TypeFamilies, MultiParamTypeClasses #-}

module System.Plugins.Criteria.LoadCriterion (LoadCriterion(..)) where

-- The 'Constraint' kind is defined in 'GHC.Exts'

import GHC.Exts
import System.Plugins.DynamicLoader

class LoadCriterion (c :: Constraint) t where
  data Criterion c t
  type Effective c t :: *
  addDynamicLibrary :: Criterion c t -> String -> IO ()
  addDynamicLibrary _ = addDLL
  resolveSymbols :: Criterion c t -> IO ()
  resolveSymbols _ = resolveFunctions
  loadQualified :: c => Criterion c t -> String -> IO (Effective c t)

