module HeartBeat where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Signal
import Signal.Channel
import Control.Monad.Aff

foreign import data HEARTBEAT       :: !
foreign import data HeartBeat       :: *

foreign import getCurrentNoteFromPlayback :: forall e. (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import loadFile :: forall e. String -> (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import getMidiFile :: forall e. String -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat

foreign import createSong :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat

foreign import play :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import pause :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import stop :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit
