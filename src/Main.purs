module Main where

import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude (bind, return, (++), show, not, unit)
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
import NoteHelper
import VexFlow
import Signal
import ClearCanvas

type AppEffects = (dom :: DOM)


nootje arg = do
  clearRect "noteHelperCanvas"
  noteHelperCanvas <- createCanvas "noteHelperCanvas"
  noteHelperRenderer <- createRenderer noteHelperCanvas 
  noteHelper <- drawHelperStaff noteHelperRenderer arg
  return unit



-- Entry point for the browser.
-- main :: forall e. State -> Eff (console :: CONSOLE, dom :: DOM, channel :: CHANNEL, err :: EXCEPTION, vexFlow :: VEXFLOW, midi :: MidiPlayer.MIDI | e) (App State Action)
main state = do
  -- Create a signal of URL changes.
  urlSignal <- sampleUrl
  --   Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)
-- start {} :: Config      
  app <- start
    { initialState: state
    , update:
      --   Logs all actions and states (removed in production builds).
      fromSimple update
    , view: view
    , inputs: [routeSignal] }

  renderToDOM "#app" app.html

  canvas <- createCanvas "notationCanvas"
  MidiPlayer.loadFile "test4.mid"
  MidiPlayer.loadPlugin { soundfontUrl : "midi/examples/soundfont/"
                        , instrument   : "acoustic_grand_piano"
                        }
    (const (MidiPlayer.getData >>= renderMidi canvas))

  --   Used by hot-reloading code in support/index.js
  -- playNote
  runSignal (app.state ~> \x -> nootje x.ui.selectedNote )
  return app

-- "noteHelperCanvas"
-- nootje :: String -

