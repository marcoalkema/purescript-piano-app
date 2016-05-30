module Qambi where

import Prelude
import Control.Monad.Eff

foreign import data QAMBI       :: !
foreign import data Qambi       :: *

foreign import loadQambi :: forall a e. Eff (qambi :: QAMBI | e ) Unit
