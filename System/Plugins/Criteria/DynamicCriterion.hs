module System.Plugins.Criteria.DynamicCriterion where

import System.Plugins.Criteria.LoadCriterion
import System.Plugins.DynamicLoader
import Data.Dynamic

instance LoadCriterion (Typeable t) t where
  data Criterion (Typeable t) t = DynamicCriterion
  type Effective (Typeable t) t = Maybe t
  loadQualified DynamicCriterion n = loadQualifiedDynFunction (adornSymbol n)
    where adornSymbol n = n ++ "Dyn"


loadQualifiedDynFunction :: Typeable t => String -> IO (Maybe t)
loadQualifiedDynFunction name = fmap fromDynamic dyn
  where dyn :: IO Dynamic
        dyn = loadQualifiedFunction name
