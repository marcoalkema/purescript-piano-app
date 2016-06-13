module AutoScroll where

import Prelude
import Control.Monad.Eff

foreign import data DOM       :: !
foreign import data Dom       :: *

foreign import scrollTo :: forall e. Eff (dom :: DOM | e) Unit
