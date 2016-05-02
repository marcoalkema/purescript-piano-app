module HeartBeat where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import App.UI

foreign import data HEARTBEAT :: !
foreign import data Heartbeat :: *

foreign import songEventListener :: forall a e. (Action -> State -> State) -> Eff (heartbeat :: HEARTBEAT | e) State
