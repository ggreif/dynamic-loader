{-# LANGUAGE KindSignatures, ConstraintKinds,
             TypeFamilies, MultiParamTypeClasses #-}

module System.Plugins.Criteria.LoadCriterion (LoadCriterion(..)) where

-- The 'Constraint' kind is defined in 'GHC.Exts'

import GHC.Exts

class LoadCriterion (c :: Constraint) t where
  data Criterion c t
  type Effective c t :: *
  loadQualified :: c => Criterion c t -> String -> IO (Effective c t)

