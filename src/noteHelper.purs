module NoteHelper where

import Prelude
import Control.Monad.Eff (Eff)
import VexFlow
import VexMusic
import MidiToVexFlow
import Data.Tuple

drawHelperStaff :: forall e. VexFlow -> MidiNote -> MidiNote -> Eff (vexFlow :: VEXFLOW | e) Unit
drawHelperStaff renderer userNote playBackNote = do
  ctx   <- createCtx renderer
  staff <- createStave 35 10 200.0
  createKeySignature "C" staff
  drawKeyStave staff "treble" ctx
  notes <- createNotes [[{ pitch : [vexNoteToVexFlowPitch $ midiNoteToVexTone userNote]
                         , duration : "4"}]
                       , [{ pitch : [vexNoteToVexFlowPitch $ midiNoteToVexTone playBackNote]
                          , duration : "4"}]]
  setColor notes [[true], [false]]
  accidentals <- addAccidentals notes $ vexBarToIndexedAccidentals $ midiNoteToVexNote userNote playBackNote
  voice <- addNotesToVoice accidentals (createNewVoice 1 4.0)
  formatter voice (200.0)
  drawVoice ctx staff voice

midiNoteToVexNote :: MidiNote -> MidiNote -> Array (Array VexNote)
midiNoteToVexNote userNote playBackNote = [ [userNote' userNote ]
                            , [userNote' playBackNote]]

userTone :: MidiNote -> VexTone
userTone note = { pitch      : fst (midiNoteToPartialVexFlowNote $ mod note 12)
                , accidental : snd (midiNoteToPartialVexFlowNote $ mod note 12)
                , octave     : midiNoteToOctave note
                }

userNote' :: MidiNote -> VexNote
userNote' note = { note     : [userTone note]
                , duration : "4"
                }
