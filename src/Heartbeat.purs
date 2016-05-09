module HeartBeat where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Signal
import Signal.Channel

foreign import data HEARTBEAT       :: !
foreign import data HeartBeat     :: *

foreign import playNote :: forall e. (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit
