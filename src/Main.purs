module Main where

import App.Routes (match)
import App.Layout
import App.UI as UI
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class 
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Console
import Data.Tuple
import Data.Foldable
import Data.Foreign
import Data.List
import Data.Function
import Data.Maybe
import DOM.Timer
import DOM (DOM)
import Prelude
import Pux
import Pux.Router (sampleUrl)
import Pux.Html (Html)
import Signal
import Signal.Channel
import VexFlow
import MidiPlayer
import MidiToVexFlow (renderMidi)
import HeartBeat
import NoteHelper
import ClearCanvas
import MidiToVexFlow
import Quantizer
import ColorNotation

type AppEffects = (dom :: DOM, heartbeat :: HEARTBEAT, console :: CONSOLE, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI, canvas :: ClearCanvas.CANVAS)
type MidiNote       = Int
type MidiNotes      = { midiNotes :: Array MidiJsTypes.MidiNote }
type Ticks          = Number

midiFile = "test4.mid"

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

  userChannel <- channel 0
  let userInputSignal       :: Signal MidiNote
      userInputSignal       = subscribe userChannel
      keyboardInputSignal   = userInputSignal ~> setCurrentKeyBoardInput

  endOfTrackChannel <- channel false
  let endOfTrackSignal :: Signal Action
      endOfTrackSignal = subscribe endOfTrackChannel ~> resetPlayback

  metronomeChannel <- channel 120
  let metronomeSignal :: Signal Action
      metronomeSignal = subscribe metronomeChannel ~> setMetronome

  leftLocatorChannel <- channel 1
  let leftLocatorSignal :: Signal Action
      leftLocatorSignal = subscribe leftLocatorChannel ~> setLeftLocator

  rightLocatorChannel <- channel 1
  let rightLocatorSignal :: Signal Action
      rightLocatorSignal = subscribe rightLocatorChannel ~> setRightLocator

  recordNotePlaybackChannel <- channel 0
  let recordNotePlaybackSignal = (~>) (dropRepeats $ subscribe recordNotePlaybackChannel)

  app <- start
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: [fromMaybe routeSignal $ mergeMany [routeSignal, playBackSignal, incrementPlayBackSignal, keyboardInputSignal, processedMidiSignal, midiEventSignal, ticksSignal, endOfTrackSignal, metronomeSignal, leftLocatorSignal, rightLocatorSignal]]
    }

  renderToDOM "#app" app.html
  
  runSignal (app.state ~> \state -> send recordNotePlaybackChannel <<< fromMaybe 0 $ Data.Array.index state.ui.userMelody state.ui.currentPlayBackNoteIndex)
  runSignal (app.state ~> \state -> drawNoteHelper (getAppFunctionality state)  state.ui.currentMidiKeyboardInput)
  loadHeartBeat midiFile (send playBackChannel) (send userChannel) (send endOfTrackChannel) (send metronomeChannel) (send leftLocatorChannel) (send rightLocatorChannel) recordNotePlaybackSignal
  runSignal (app.state ~> \state -> draw state.ui.currentPlayBackNoteIndex state.ui.midiEvents state.ui.colorNotation)

  return app

getAppFunctionality :: State -> Int
getAppFunctionality s = if s.ui.recordButtonPressed then
                          s.ui.currentUserMelodyHead
                        else
                          s.ui.currentPlayBackNote

-- TODO create Canvas *once*, and clear repeatedly; clearCanvas should take a Canvas value instead of a String
draw i midi notationHasColor = do
  clearCanvas "notationCanvas"
  canvas <- createCanvas "notationCanvas"
  renderMidi canvas i notationHasColor midi 
  return unit

loadMidi :: forall e. Eff (midi :: MIDI, channel :: CHANNEL | e) { midi :: Channel (Array UnsafeMidiData), ticks :: Channel Ticks}
loadMidi = do
  midiDataChannel <- channel []
  ticksChannel    <- channel 0.0
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin
    { soundfontUrl : "midi/examples/soundfont/"
    , instrument   : "acoustic_grand_piano" }
    (const $ MidiPlayer.getData2 (send midiDataChannel) (send ticksChannel))
  return { midi  : midiDataChannel
         , ticks : ticksChannel }

drawNoteHelper playBackNote userNote = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer playBackNote userNote
  return unit

getMidiNotes = map (_.noteNumber)

processMidi :: Array UnsafeMidiData -> MidiNotes
processMidi midiData = do
  let safeData  :: List MidiJsTypes.MidiEvent
      safeData  = toList $ map unsafeF1 midiData
      midiNotes :: Array MidiJsTypes.MidiNote
      midiNotes = toUnfoldable $ Data.List.filter (\note -> note.noteNumber > 0)
                  <<< map (quantizeNote 1000.0 0.0)
                  <<< duration
                  <<< map (\midiObject -> Tuple midiObject false)
                  <<< Data.List.filter filterNotes
                  $ toList safeData
  { midiNotes }

setCurrentKeyBoardInput :: MidiNote -> App.Layout.Action
setCurrentKeyBoardInput = Child <<< UI.SetMidiKeyBoardInput

incrementPlayIndex :: Int -> App.Layout.Action
incrementPlayIndex _ = Child (UI.IncrementPlayBackIndex)

setCurrentPlayBackNote :: MidiNote -> App.Layout.Action
setCurrentPlayBackNote = Child <<< UI.SetPlayBackNote

setMidiData :: (Array MidiNote) -> App.Layout.Action
setMidiData = Child <<< UI.SetMidiData

setTicks :: Number -> App.Layout.Action
setTicks = Child <<< UI.SetTicks

setMidiEvent :: Array Foreign -> App.Layout.Action
setMidiEvent = Child <<< UI.SetMidiEvent

resetPlayback :: Boolean -> App.Layout.Action
resetPlayback _ = Child UI.ResetPlayback

setMetronome :: Int -> App.Layout.Action
setMetronome = Child <<< UI.TempoSliderChanged

setLeftLocator :: Int -> App.Layout.Action
setLeftLocator = Child <<< UI.SetLeftLocatorSlider

setRightLocator :: Int -> App.Layout.Action
setRightLocator = Child <<< UI.SetRightLocatorSlider
