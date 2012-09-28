{-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies,
             MultiParamTypeClasses, FlexibleInstances #-}

module System.Plugins.Criteria.UnsafeCriterion (Criterion(..)) where

import System.Plugins.Criteria.LoadCriterion
import System.Plugins.DynamicLoader

instance LoadCriterion () t where
  data Criterion () t = UnsafeCriterion
  type Effective () t = IO t
  loadQualified UnsafeCriterion = loadQualifiedFunction
