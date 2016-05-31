module ClearCanvas where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

foreign import data CANVAS     :: !
foreign import data Canvas     :: *

foreign import clearRect :: forall e. String -> Eff (canvas :: CANVAS | e) Unit

foreign import clearCanvas :: forall e. String -> Eff (canvas :: CANVAS | e) Unit
