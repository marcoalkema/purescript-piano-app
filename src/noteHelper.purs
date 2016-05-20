module NoteHelper where

import Prelude
import Control.Monad.Eff (Eff)
import VexFlow
import VexMusic
import MidiToVexFlow

type Aap = { fst :: Int
           , snd :: Int }

drawHelperStaff :: forall e. VexFlow -> Aap -> Eff (vexFlow :: VEXFLOW | e) Unit
drawHelperStaff renderer notes = do
  ctx   <- createCtx renderer
  staff <- createStave 35 10 200.0
  createKeySignature "C" staff
  drawKeyStave staff "treble" ctx
  notes <- createNotes [[{ pitch : [vexNoteToVexFlowPitch $ midiNoteToVexTone notes.snd]
                         , duration : "4"}]
                       , [{ pitch : [vexNoteToVexFlowPitch $ midiNoteToVexTone notes.fst]
                          , duration : "4"}]]
  setColor notes [[true, false], []]
  accidentals <- addAccidentals notes [[[]]]
  voice <- addNotesToVoice notes (createNewVoice 1 4.0)
  formatter voice (200.0)
  drawVoice ctx staff voice
  
