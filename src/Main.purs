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

<<<<<<< HEAD
  playBackChannel <- playBackNoteSignal
  let trackSubscription :: Signal MidiNote
      trackSubscription       = subscribe playBackChannel
      incrementPlayBackSignal = trackSubscription ~> incrementPlayIndex 
      playBackSignal          = trackSubscription ~> setCurrentPlayBackNote


  userChannel <- userNoteSignal
  let userInputSubscription :: Signal MidiNote
      userInputSubscription = subscribe userChannel
      userInputSignal       = userInputSubscription ~> setCurrentKeyBoardInput 
      triggerSignal         = userInputSubscription ~> \midiNote -> setUserMelody
  runSignal (userInputSubscription ~> \midiNote -> MidiPlayer.logger midiNote)

  endOfTrackChannel <- endOfTrackSignal
  let endOfTrackSubscription = subscribe endOfTrackChannel
      endOfTrackSignal :: Signal Action
      endOfTrackSignal = endOfTrackSubscription ~> resetPlayback
=======
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
>>>>>>> a4c1e2a6a89f8aa0685c166890dd3fc0e0b0d409
  
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

<<<<<<< HEAD
draw i midi notationHasColor = do
=======
-- TODO create Canvas *once*, and clear repeatedly; clearCanvas should take a Canvas value instead of a String
draw i midi notationHasColor= do
>>>>>>> a4c1e2a6a89f8aa0685c166890dd3fc0e0b0d409
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
<<<<<<< HEAD
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano" }
    (const $ MidiPlayer.getData2 mail mail2)
  return midiChannels

-- processForeign d = do
--   ticksPerBeat <- getTicksPerBeat
  
  


-- playBackNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
playBackNoteSignal = do 
  chan <- channel 0
  let mail = send chan
  return chan

endOfTrackSignal = do 
  chan <- channel false
  return chan

-- userNoteSignal :: forall e. Eff (heartbeat :: HEARTBEAT, channel :: CHANNEL | e) (Channel MidiNote)
userNoteSignal = do 
  chan <- channel 0
  let mail = send chan
  return chan

midiDataSignal :: forall e. Eff (midi :: MidiPlayer.MIDI, channel :: CHANNEL | e)
 (Channel (Array Foreign))
midiDataSignal = channel []

=======
  MidiPlayer.loadPlugin
    { soundfontUrl : "midi/examples/soundfont/"
    , instrument   : "acoustic_grand_piano" }
    (const $ MidiPlayer.getData2 (send midiDataChannel) (send ticksChannel))
  return { midi: midiDataChannel, ticks: ticksChannel }
>>>>>>> a4c1e2a6a89f8aa0685c166890dd3fc0e0b0d409

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
