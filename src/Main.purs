module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class 
import DOM (DOM)
import Prelude (bind, return, (++), show, not, unit, ($), (<$>), (<<<), map)
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

midiFile = "test4.mid"

drawNoteHelper userNote  = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas   <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper         <- drawHelperStaff noteHelperRenderer userNote
  return unit

toAction :: Int -> App.Layout.Action
toAction n = Child (UI.Piano n)

increment :: Int -> App.Layout.Action
increment n = Child (UI.IncrementPlayBackIndex)

playBackNote :: Int -> App.Layout.Action
playBackNote n = Child (UI.UserNote n)

-- Entry point for the browser.
-- main :: forall e. State -> Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) (App State Action)
main state = do
  urlSignal <- sampleUrl

  chan2 <- playBackNoteSignal
  let sub2        = subscribe chan2
      midiAction2 = (<~) increment (sub2 ~> \x -> x)
      midiAction3 = (<~) playBackNote (sub2 ~> \x -> x)
  runSignal (sub2 ~> \x -> MidiPlayer.logger x)

  chan <- userNoteSignal
  let sub        = subscribe chan
      midiAction = (<~) toAction (sub ~> \x -> x)
  runSignal (sub ~> \x -> MidiPlayer.logger x)
    
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  app <- start
    { initialState: state
    , update:
      --update receives layout.update :: Action -> State -> State
      fromSimple update
      -- view receives layout.view :: State :: Html Action
    , view: view
    , inputs: [fromJust $ mergeMany [routeSignal, midiAction, midiAction2, midiAction3]]
    }

  renderToDOM "#app" app.html
  let appState :: { html :: Signal (Html Action)
                  , state :: Signal App.Layout.State }
      appState = app

      currentAppState = runSignal (app.state ~> \x -> MidiPlayer.logger x)

  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile midiFile
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano"
                        }
    (const (MidiPlayer.getData >>= renderMidi canvas))

  runSignal (app.state ~> \x -> drawNoteHelper x.ui.currentPlayBackNotes)
  -- runSignal (app.state ~> \x -> drawNoteHelper x.ui.selectedNote)
  runSignal (sub ~> \userNote -> drawNoteHelper userNote)
  return app

-- playBackNoteSignal :: forall e. Eff (heartBeat :: HEARTBEAT, channel :: CHANNEL | e) Unit
playBackNoteSignal = do 
  chan1 <- channel 0
  let sub1 :: Signal Int
      sub1  = subscribe chan1
      mail1 :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      mail1 = send chan1
      send1 :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      send1 a = (mail1 a)
  HeartBeat.loadFile midiFile send1
  return chan1

-- userNoteSignal :: forall e. Eff (heartBeat :: HEARTBEAT, channel :: CHANNEL | e) Unit
userNoteSignal = do 
  chan <- channel 0
  let sub :: Signal Int
      sub  = subscribe chan
      mail :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      mail = send chan
      send' :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      send' a = (mail a)
  MidiPlayer.logger "MAUWMAUWMAUW"
  HeartBeat.getCurrentNoteFromPlayback send'
  return chan

-- start :: forall state action eff. Config state action eff -> Eff (CoreEffects eff) (App state action)
-- start config = do
--   actionChannel <- channel Nil
--   let actionSignal :: Array (Signal Action)
--       actionSignal = subscribe actionChannel
--       -- config.inputs :: [Signal Action]
--       input :: (Signal Action)
--       input = fromJust $ mergeMany $
--         reverse (actionSignal : map (map singleton) (fromFoldable $ config.inputs))
--       foldState :: (EffModel s a e) -> Action -> Update state action eff
--       foldState effModel action = config.update action effModel.state
--       foldActions :: List Action -> (EffModel s a e) -> (EffModel s a e)
--       foldActions actions effModel =
--         foldl foldState (noEffects effModel.state) actions
--       effModelSignal :: Signal (EffModel s a e)
--       effModelSignal =
--         foldp foldActions (noEffects config.initialState) input
-- ---     ^     ^           ^          ^                    ^
-- ---     |     |           |          |                    |
-- ---     |     |           |          |                    Signal Action 
-- ---     |     |           |          UI-State
-- ---     |     |           |          
-- ---     |     |           EffModel s a e
-- ---     |     |           
-- ---     |     (a -> (EffModel s a e) -> (EffModel s a e))
-- ---     |    
-- ---     (a -> (EffModel s a e) -> (EffModel s a e)) -> (EffModel s a e) -> (Signal a) -> (Signal (EffModel s a e))
--       stateSignal :: Signal state
--       stateSignal = effModelSignal ~> _.state
--       -- htmlSignal :: forall eff. Fn3 (Action -> Eff eff Unit) (Action -> Action) (Html Action) (Html Action)
--       htmlSignal :: Html Action
--       htmlSignal = stateSignal ~> \state ->
--         (runFn3 render) (send actionChannel <<< singleton) (\a -> a) (config.view state)
-- ---                      ^-> [Signal Action]                              ^-> (state -> HTML Action)
--       mapAffect :: Eff (err :: EXCEPTION | e) Unit
-- --    affect ::  Aff e a
--       mapAffect affect = launchAff $ do
--         action <- later affect
--         liftEff $ send actionChannel (singleton action)
--       -- effectsSignal :: Array (Aff (channel :: CHANNEL | eff) Action
--       effectsSignal = effModelSignal ~> map mapAffect <<< _.effects
--   runSignal $ effectsSignal ~> sequence_
--   pure $ { html: htmlSignal, state: stateSignal }
--   where bind = Prelude.bind

