module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class 
import DOM (DOM)
import Prelude (bind, return, (++), show, not, unit, ($), (<$>), (<<<), map, (<>))
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

type AppEffects = (dom :: DOM)
type MidiNote = Int

-- Entry point for the browser.
main :: forall e. State -> Eff (heartbeat :: HEARTBEAT, console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI, canvas :: ClearCanvas.CANVAS | e) (App State Action)
main state = do
  urlSignal <- sampleUrl
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  playBackChannel <- playBackNoteSignal
  let trackSubscription :: Signal MidiNote
      trackSubscription       = subscribe playBackChannel
      incrementPlayBackSignal = trackSubscription ~> \midiNote -> incrementPlayIndex midiNote
      playBackSignal          = trackSubscription ~> \midiNote -> playBackAction midiNote

  userChannel <- userNoteSignal
  let userInputSubscription :: Signal MidiNote
      userInputSubscription = subscribe userChannel
      userInputSignal       = userInputSubscription ~> \midiNote -> toPianoAction midiNote
  runSignal (userInputSubscription ~> \midiNote -> MidiPlayer.logger midiNote)
     
  app <- start
    { initialState: state
    , update:
      fromSimple update
    , view: view
    , inputs: [fromJust $ mergeMany [routeSignal, userInputSignal, playBackSignal]]
    }
    
  renderToDOM "#app" app.html

  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano" }
    (const (MidiPlayer.getData >>= renderMidi canvas))

  runSignal (app.state ~> \state -> drawNoteHelper { fst : state.ui.currentPlayBackNotes,
                                                     snd : state.ui.selectedNote } )
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

drawNoteHelper notes = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer notes
  return unit

toPianoAction :: MidiNote -> App.Layout.Action
toPianoAction n = Child (UI.Piano n)

incrementPlayIndex :: Int -> App.Layout.Action
incrementPlayIndex n = Child (UI.IncrementPlayBackIndex)

playBackAction :: Int -> App.Layout.Action
playBackAction n = Child (UI.UserNote n)
