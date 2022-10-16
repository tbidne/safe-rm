--- @since 0.1

-- | Provides the 'MaxRuns' data type.
module SafeRm.MaxRuns
  ( MaxRuns (..),
    maxRunsIngredient,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Tagged (Tagged (Tagged))
import Hedgehog (TestLimit)
import Test.Tasty qualified as T
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Options
  ( IsOption
      ( defaultValue,
        optionHelp,
        optionName,
        parseValue
      ),
    OptionDescription (Option),
  )
import Text.Read qualified as TR

-- | Sets the maximum successful runs for each test.
--
-- @since 0.1
newtype MaxRuns = MkMaxRuns
  { -- | @since 0.1
    unMaxRuns :: TestLimit
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Num
    )
    via TestLimit

-- | @since 0.1
instance IsOption MaxRuns where
  defaultValue = MkMaxRuns 100
  parseValue = readLimit
  optionName = Tagged "max-runs"
  optionHelp = Tagged "The maximum number of runs for each test."

-- | Ingredient for 'MaxRuns'.
--
-- @since 0.1
maxRunsIngredient :: Ingredient
maxRunsIngredient = T.includingOptions [Option @MaxRuns Proxy]

readLimit :: String -> Maybe MaxRuns
readLimit = fmap (fromIntegral @Int) . TR.readMaybe
