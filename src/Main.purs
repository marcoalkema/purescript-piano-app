module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class 
import DOM (DOM)
import Prelude (bind, return, (++), show, not, unit, ($), (<$>), (<<<), map, (<>), (==))
import Pux
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console
import Prelude (Unit, (>>=), const, bind)
import VexFlow (VEXFLOW, createCanvas)
import MidiPlayer
import MidiToVexFlow (renderMidi)
import HeartBeat
import Data.Foldable
import NoteHelper
import VexFlow
import Signal
import ClearCanvas
import Data.List
import Data.Function
import App.UI as UI
import Pux.Html (Html)
import App.Layout
import Control.Monad.Aff
import Data.Maybe

type AppEffects = (dom :: DOM)
type MidiNote = Int

-- Entry point for the browser.
main :: forall e. State -> Eff (heartbeat :: HEARTBEAT, console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI, canvas :: ClearCanvas.CANVAS | e) (App State Action)
main state = do
  urlSignal <- sampleUrl
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  -- playBackChannel <- playBackNoteSignal
  -- let trackSubscription :: Signal MidiNote
  --     trackSubscription       = subscribe playBackChannel
  --     incrementPlayBackSignal = trackSubscription ~> \midiNote -> incrementPlayIndex midiNote
  --     playBackSignal          = trackSubscription ~> \midiNote -> playBackAction midiNote


  userChannel <- userNoteSignal
  let userInputSubscription :: Signal MidiNote
      userInputSubscription = subscribe userChannel
      userInputSignal       = userInputSubscription ~> \midiNote -> toPianoAction midiNote
      -- triggerSignal2        = userInputSubscription ~> \midiNote -> trigger2
      triggerSignal         = userInputSubscription ~> \midiNote -> trigger

      
  app <- start
    { initialState: state
    , update:
      fromSimple update
    , view: view
    , inputs: [fromJust $ mergeMany [routeSignal, userInputSignal, triggerSignal]]
    }

  renderToDOM "#app" app.html

  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano" }
    (const (MidiPlayer.getData >>= (renderMidi canvas app.state)))

  -- let aap = runSignal (app.state ~> \state -> matchUserInput state.ui.userNote state.ui.userMelody)
  runSignal (app.state ~> \state -> drawNoteHelper state.ui.currentPlayBackNote state.ui.selectedNote )
  -- runSignal (app.state ~> \state -> MidiPlayer.logger state.ui.melody )
  -- runSignal (app.state ~> \x -> drawNoteHelper x.ui.selectedNote)
  -- runSignal (userInputSubscription ~> \userNote -> drawNoteHelper userNote)


  return app

playBackNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
playBackNoteSignal = do 
  chan <- channel 0
  let sub  = subscribe chan
      mail = send chan
  HeartBeat.loadFile midiFile mail
  return chan

userNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
userNoteSignal = do 
  chan <- channel 0
  let sub  = subscribe chan
      mail = send chan
  HeartBeat.getCurrentNoteFromPlayback mail
  return chan

midiFile = "test4.mid"

drawNoteHelper playBackNote userNote = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer playBackNote userNote
  return unit

toPianoAction :: MidiNote -> App.Layout.Action
toPianoAction n = Child (UI.Piano n)

incrementPlayIndex :: Int -> App.Layout.Action
incrementPlayIndex n = Child (UI.IncrementPlayBackIndex)

playBackAction :: Int -> App.Layout.Action
playBackAction n = Child (UI.UserNote n)

trigger :: App.Layout.Action
trigger = Child (UI.SetUserMelody)

trigger2 :: App.Layout.Action
trigger2 = Child (UI.SetUserMelody2)

-- matchUserInput :: MidiNote -> Array MidiNote -> App.Layout.Action
-- matchUserInput userNote playBackNotes = if currentNote == Nothing then
--                                           Child (UI.SetUserMelody NoteHelper.melody)
--                                         else if (Just userNote) == currentNote then
--                                           Child (UI.SetUserMelody $ fromJust $ Data.Array.tail playBackNotes)
--                                         else
--                                           Child (UI.SetUserMelody playBackNotes)
--   where
--     currentNote = Data.Array.head playBackNotes
