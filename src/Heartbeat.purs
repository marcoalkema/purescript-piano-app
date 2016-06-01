module HeartBeat where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Signal
import Signal.Channel
import Control.Monad.Aff
import Control.Monad.Eff.Exception

foreign import data HEARTBEAT       :: !
foreign import data HeartBeat       :: *

foreign import loadHeartBeat :: forall e. String -> (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) ->  (Boolean -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat
