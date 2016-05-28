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

foreign import getUserInput :: forall a e. (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> a ->  Eff (exception :: EXCEPTION, channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import loadFile :: forall a e. String -> (Int -> Eff (channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit) -> a -> Eff (exception :: EXCEPTION, channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import getMidiFile :: forall e. String -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat

foreign import createSong :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat

foreign import play :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import pause :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import stop :: forall e. HeartBeat -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import playNote :: forall a e. Int  -> a -> Eff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) Unit

foreign import getSequencer :: forall e. Aff ( channel :: CHANNEL, heartbeat :: HEARTBEAT | e ) HeartBeat
