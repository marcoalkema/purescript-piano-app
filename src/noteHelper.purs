module NoteHelper where

import Prelude
import Control.Monad.Eff (Eff)
import VexFlow
import VexMusic
import MidiToVexFlow

drawHelperStaff :: forall e. VexFlow -> Int -> Eff (vexFlow :: VEXFLOW | e) Unit
drawHelperStaff renderer arg  = do
  ctx   <- createCtx renderer
  staff <- createStave 35 10 200.0
  createKeySignature "C" staff
  drawKeyStave staff "treble" ctx
  notes <- createNotes [[{ pitch : [vexNoteToVexFlowPitch $ midiNoteToVexTone arg]
                         , duration : "4"} ]]
  accidentals <- addAccidentals notes [[[]]]
  voice <- addNotesToVoice accidentals (createNewVoice 1 4.0)
  formatter voice (200.0)
  drawVoice ctx staff voice
  
