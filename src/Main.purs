module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff
import DOM.Timer
import Control.Monad.Eff.Class 
import DOM (DOM)
-- import Prelude (bind, return, (++), show, not, unit, ($), (<$>), (<<<), map, (<>), (==), pure, (>))
import Prelude
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
import Data.Either
import MidiToVexFlow
import Quantizer
import Data.Tuple
import Data.Foreign
import ColorNotation
import Qambi

type AppEffects = (dom :: DOM)
type MidiNote = Int

-- Entry point for the browser.
-- main :: forall e. State -> Eff (heartbeat :: HEARTBEAT, console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI, canvas :: ClearCanvas.CANVAS | e) (App State Action)
main state = do
  
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano" }
    (const $ logger "hoi")
    
  urlSignal <- sampleUrl
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  playBackChannel <- playBackNoteSignal
  let trackSubscription :: Signal MidiNote
      trackSubscription       = subscribe playBackChannel
      incrementPlayBackSignal = trackSubscription ~> incrementPlayIndex 
      playBackSignal          = trackSubscription ~> setCurrentPlayBackNote
  runSignal (trackSubscription ~> MidiPlayer.logger)
  -- runSignal (trackSubscription ~> \midiNote -> playNote midiNote sequencer)

  userChannel <- userNoteSignal
  let userInputSubscription :: Signal MidiNote
      userInputSubscription = subscribe userChannel
      userInputSignal       = userInputSubscription ~> setCurrentKeyBoardInput 
      triggerSignal         = userInputSubscription ~> \midiNote -> setUserMelody
  runSignal (userInputSubscription ~> \midiNote -> MidiPlayer.logger midiNote)

  -- midiChannel <- midiDataSignal
  -- let midiDataSubscription :: Signal (Array Foreign)
  --     midiDataSubscription = subscribe midiChannel
  --     midiDataSignal       = midiDataSubscription ~> \dat -> logger dat
  
  app <- start
    { initialState: state
    , update:
      fromSimple update
    , view: view
    , inputs: [fromJust $ mergeMany [routeSignal, playBackSignal, incrementPlayBackSignal, userInputSignal, triggerSignal]]
    }

  renderToDOM "#app" app.html

  runSignal (app.state ~> \state -> drawNoteHelper state.ui.currentPlayBackNote state.ui.currentMidiKeyboardInput )
  loadHeartBeat midiFile (send playBackChannel) (send userChannel)
  runSignal (app.state ~> \state -> draw state.ui.currentPlayBackNoteIndex)
  
  return app

draw n = do
  clearCanvas "notationCanvas"
  canvas <- createCanvas "notationCanvas"
  midiData <- MidiPlayer.getData
  renderMidi canvas n midiData
  return unit


-- playBackNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
playBackNoteSignal = do 
  chan <- channel 0
  let mail = send chan
  return chan

-- userNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
userNoteSignal = do 
  chan <- channel 0
  let mail = send chan
  return chan

-- midiDataSignal :: forall e. Eff (midi :: MidiPlayer.MIDI, channel :: CHANNEL | e)
 -- (Channel MidiNote)
midiDataSignal  = do
  chan <- channel []
  let mail = send chan
  -- MidiPlayer.loadFile2 midiFile (MidiPlayer.getData2 send)
  return chan

midiFile = "colorTest4.mid"

drawNoteHelper playBackNote userNote = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer playBackNote userNote
  return unit

processMidi midiData = do
  ticksPerBeat <- getTicksPerBeat
  let safeData  = toList $ map unsafeF1 midiData
      numerator = getNumerator safeData
      midiNotes = Data.List.filter (\x -> x.noteNumber > 0)
                  <<< map (quantizeNote ticksPerBeat 0.0)
                  <<< calculateDuration
                  <<< map (\midiObject -> Tuple midiObject false) -- midiEventWriter
                  <<< Data.List.filter filterNotes
                  $ toList safeData
  return midiNotes

setCurrentKeyBoardInput :: MidiNote -> App.Layout.Action
setCurrentKeyBoardInput n = Child (UI.SetMidiKeyBoardInput n)

incrementPlayIndex :: Int -> App.Layout.Action
incrementPlayIndex n = Child (UI.IncrementPlayBackIndex)

setCurrentPlayBackNote :: MidiNote -> App.Layout.Action
setCurrentPlayBackNote n = Child (UI.SetPlayBackNote n)

setUserMelody :: App.Layout.Action
setUserMelody = Child (UI.SetUserMelody)

setMidiData :: (Array Foreign) -> App.Layout.Action
setMidiData f = Child (UI.SetMidiData f) 
