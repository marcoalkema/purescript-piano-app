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
import MidiJS as MidiPlayer
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
-- import Signal.Loop

type AppEffects = (dom :: DOM)


nootje arg = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper <- drawHelperStaff noteHelperRenderer arg
  return unit

toAction :: Int -> App.Layout.Action
toAction n = Child (UI.Piano n)

-- Entry point for the browser.
-- main :: forall e. State -> Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) (App State Action)
main state = do
  urlSignal <- sampleUrl

  chan <- bar
  let sub = subscribe chan
  runSignal (sub ~> \x -> MidiPlayer.logger x)
  let midiAction = (<~) toAction (sub ~> \x -> x)

  -- let foo = mergeMany [urlSignal, aap]
  let routeSignal :: Signal Action
      routeSignal = urlSignal ~> \r -> PageView (match r)

  -- Child Piano n state
  -- app.state

  app <- start
    { initialState: state
    , update:
      --update receives layout.update :: Action -> State -> State
      fromSimple update
      -- view receives layout.view :: State :: Html Action
    , view: view
    , inputs: [merge routeSignal midiAction] }

  renderToDOM "#app" app.html
  let appState :: { html :: Signal (Html Action)
                  , state :: Signal App.Layout.State }
      appState = app

      currentAppState = runSignal (app.state ~> \x -> MidiPlayer.logger x)
  currentAppState

  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile "test4.mid"
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano"
                        }
    (const (MidiPlayer.getData >>= renderMidi canvas))

  runSignal (app.state ~> \x -> nootje x.ui.selectedNote)
  runSignal (sub ~> \x -> nootje x)
  runSignal (routeSignal ~> \x -> MidiPlayer.logger x)

  
  return app



-- foo :: forall e. Eff (channel :: CHANNEL | e) Unit
bar = do 
  chan <- channel 0
  let sub :: Signal Int
      sub  = subscribe chan
      mail :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      mail = send chan
      send' :: forall e. Int -> Eff (channel :: CHANNEL | e) Unit
      send' a = (mail a)
  playNote send'
  -- runSignal (sub ~> \x -> MidiPlayer.logger x)
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

