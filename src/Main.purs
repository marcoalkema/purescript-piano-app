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

type AppEffects = (dom :: DOM)
type MidiNote = Int

-- Entry point for the browser.
-- main :: forall e. State -> Eff (heartbeat :: HEARTBEAT, console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI, canvas :: ClearCanvas.CANVAS | e) (App State Action)
main state = do
  midiDataChannels <- loadMidi
  let midiDataSignal :: Signal (Array Foreign)
      midiDataSignal = subscribe midiDataChannels.midi
      ticksSignal :: Signal Action
      ticksSignal = subscribe midiDataChannels.ticks ~> setTicks
      processedMidiSignal :: Signal Action
      processedMidiSignal = midiDataSignal ~> setMidiData <<< getMidiNotes <<< (_.midiNotes) <<< processMidi
      midiEventSignal :: Signal Action
      midiEventSignal = midiDataSignal ~> setMidiEvent

  urlSignal <- sampleUrl
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  playBackChannel <- channel 0
  let trackSignal :: Signal MidiNote
      trackSignal = subscribe playBackChannel
      incrementPlayBackSignal = trackSignal ~> incrementPlayIndex
      playBackSignal          = trackSignal ~> setCurrentPlayBackNote
  runSignal (trackSignal ~> MidiPlayer.logger)

  userChannel <- channel 0
  let userInputSignal       :: Signal MidiNote
      userInputSignal       = subscribe userChannel
      keyboardInputSignal   = userInputSignal ~> setCurrentKeyBoardInput
      triggerSignal         = userInputSignal ~> \midiNote -> setUserMelody
  runSignal (userInputSignal ~> MidiPlayer.logger)

  endOfTrackChannel <- channel false
  let endOfTrackSignal :: Signal Action
      endOfTrackSignal = subscribe endOfTrackChannel ~> resetPlayback
  
  app <- start
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [fromMaybe routeSignal $ mergeMany [routeSignal, playBackSignal, incrementPlayBackSignal, keyboardInputSignal, triggerSignal, processedMidiSignal, midiEventSignal, ticksSignal, endOfTrackSignal]]
    }

  renderToDOM "#app" app.html

  runSignal (app.state ~> \state -> drawNoteHelper state.ui.currentPlayBackNote state.ui.currentMidiKeyboardInput)
  loadHeartBeat midiFile (send playBackChannel) (send userChannel) (send endOfTrackChannel)
  runSignal (app.state ~> \state -> draw state.ui.currentPlayBackNoteIndex state.ui.midiEvents state.ui. colorNotation)
  
  return app

-- TODO create Canvas *once*, and clear repeatedly; clearCanvas should take a Canvas value instead of a String
draw i midi notationHasColor= do
  clearCanvas "notationCanvas"
  canvas <- createCanvas "notationCanvas"
  renderMidi canvas i notationHasColor midi 
  return unit

-- TODO use a type alias instead of Foreign
loadMidi :: forall e. Eff (midi :: MIDI, channel :: CHANNEL | e) { midi :: Channel (Array Foreign), ticks :: Channel Number }
loadMidi = do
  midiDataChannel <- channel []
  ticksChannel <- channel 0.0
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin
    { soundfontUrl : "midi/examples/soundfont/"
    , instrument   : "acoustic_grand_piano" }
    (const $ MidiPlayer.getData2 (send midiDataChannel) (send ticksChannel))
  return { midi: midiDataChannel, ticks: ticksChannel }

midiFile = "colorTest4.mid"

drawNoteHelper playBackNote userNote = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer playBackNote userNote
  return unit

type MidiNotes = { midiNotes :: Array MidiJsTypes.MidiNote }

getMidiNotes = map (_.noteNumber)

processMidi :: Array Foreign -> MidiNotes
processMidi midiData = do
  let safeData  :: List MidiJsTypes.MidiEvent
      safeData  = toList $ map unsafeF1 midiData
      midiNotes :: Array MidiJsTypes.MidiNote
      midiNotes = toUnfoldable $ Data.List.filter (\x -> x.noteNumber > 0)
                  <<< map (quantizeNote 1000.0 0.0)
                  <<< calculateDuration
                  <<< map (\midiObject -> Tuple midiObject false) -- midiEventWriter
                  <<< Data.List.filter filterNotes
                  $ toList safeData
  { midiNotes }

setCurrentKeyBoardInput :: MidiNote -> App.Layout.Action
setCurrentKeyBoardInput = Child <<< UI.SetMidiKeyBoardInput

incrementPlayIndex :: Int -> App.Layout.Action
incrementPlayIndex _ = Child (UI.IncrementPlayBackIndex)

setCurrentPlayBackNote :: MidiNote -> App.Layout.Action
setCurrentPlayBackNote = Child <<< UI.SetPlayBackNote

setUserMelody :: App.Layout.Action
setUserMelody = Child (UI.SetUserMelody)

setMidiData :: (Array MidiNote) -> App.Layout.Action
setMidiData = Child <<< UI.SetMidiData

setTicks :: Number -> App.Layout.Action
setTicks = Child <<< UI.SetTicks

setMidiEvent :: Array Foreign -> App.Layout.Action
setMidiEvent = Child <<< UI.SetMidiEvent

resetPlayback :: Boolean -> App.Layout.Action
resetPlayback _ = Child (UI.ResetPlayback)
