module App.UI where

import Prelude
import Prelude (map)
import Math
import Math (max)
import Data.Int (round, toNumber)
import Data.Array
import Data.Foldable
import Pux.Html as Pux
import Pux.Html.Events
import Pux.Html.Elements
import Pux.Html.Elements (span)
import Pux.Html.Attributes
import Pux.Html.Attributes (style)
import MidiPlayer
import NoteHelper
import Data.Maybe
import VexFlow

--kip

data Action = PlayButtonPressed | PauseButtonPressed | LoopButtonPressed | MuteButtonPressed | NoteHelperResize | IncrementPlayBackIndex | SetUserMelody | ResetMelody | SetMidiKeyBoardInput MidiNote | PianoKeyPressed Note Octave | SetPlayBackNote MidiNote

data Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type Octave              = Int
type MidiNote            = Int
type CurrentPlayBackNote = MidiNote
type CurrentUserNote     = MidiNote


-- TODO: Separate states for ddifferent domains --> Create new / domain-specific states!!!
-- Replace some MidiNotes for Array MidiNote
type State = { currentMidiKeyboardInput :: MidiNote
             , currentUIPianoSelection  :: MidiNote
             , currentPlayBackNote      :: MidiNote
             , currentPlayBackNoteIndex :: Int
               
             , currentPlayBackMelody    :: Array MidiNote
             , userMelody               :: Array MidiNote
             , currentUserMelodyHead    :: MidiNote
               
             , noteHelperActivated      :: Boolean
             , playButtonPressed        :: Boolean
             , pauseButtonPressed       :: Boolean
             , loopButtonPressed        :: Boolean
             , muteButtonPressed        :: Boolean}

update :: Action -> State -> State

update (SetMidiKeyBoardInput n) state = state { currentMidiKeyboardInput = n }
update (PianoKeyPressed x y) state    = state { currentUIPianoSelection  = toMidiNote x y }
update (SetPlayBackNote n) state      = state { currentPlayBackNote      = n }
update IncrementPlayBackIndex state   = state { currentPlayBackNoteIndex = state.currentPlayBackNoteIndex + 1 }

update SetUserMelody state            = setMelody state
update ResetMelody state              = state { userMelody = state.userMelody }

update PlayButtonPressed state      = state { playButtonPressed        = not state.playButtonPressed  }
update PauseButtonPressed state     = state { pauseButtonPressed       = not state.pauseButtonPressed }
update LoopButtonPressed state      = state { loopButtonPressed        = not state.loopButtonPressed  }
update MuteButtonPressed state      = state { muteButtonPressed        = not state.muteButtonPressed  }
update NoteHelperResize state       = state { noteHelperActivated      = not state.noteHelperActivated }


init :: State
init = { currentMidiKeyboardInput : 0
       , currentUIPianoSelection  : 48
       , currentPlayBackNote      : fromJust $ Data.Array.head NoteHelper.melody
       , currentPlayBackNoteIndex : 0

       , currentPlayBackMelody    : NoteHelper.melody
       , userMelody               : NoteHelper.melody
       , currentUserMelodyHead    : fromJust $ Data.Array.head NoteHelper.melody
         
       , noteHelperActivated      : true
       , playButtonPressed        : false
       , pauseButtonPressed       : false
       , loopButtonPressed        : false
       , muteButtonPressed        : false }

setMelody :: State -> State
setMelody state = { currentMidiKeyboardInput : state.currentMidiKeyboardInput
                  , currentUIPianoSelection  : state.currentUIPianoSelection
                  , currentPlayBackNote      : state.currentPlayBackNote
                  , currentPlayBackNoteIndex : state.currentPlayBackNoteIndex

                  , currentPlayBackMelody    : state.currentPlayBackMelody
                  , userMelody               : newMelody 
                  , currentUserMelodyHead    : fromJust $ Data.Array.head newMelody
                    
                  , noteHelperActivated      : state.noteHelperActivated
                  , playButtonPressed        : state.playButtonPressed
                  , pauseButtonPressed       : state.pauseButtonPressed
                  , loopButtonPressed        : state.loopButtonPressed
                  , muteButtonPressed        : state.muteButtonPressed }
  where
    newMelody = matchUserInput state.currentMidiKeyboardInput state.userMelody

matchUserInput :: MidiNote -> Array MidiNote -> Array MidiNote
matchUserInput userNote playBackNotes = if currentNote == Nothing then
                                          NoteHelper.melody
                                        else if (Just userNote) == currentNote then
                                          fromJust $ Data.Array.tail playBackNotes
                                        else
                                          playBackNotes
  where
    currentNote = Data.Array.head playBackNotes

view :: State -> Html Action
view state  = do
  Pux.div
    [ style { height : "100%"
            , width : "100%"
            , overflow : "hidden" }
    , id_ "main"
    ]
    [ Pux.div [ pianoStyle
              , id_ "foo" ] (foldr (\y x -> cons y x) (drawOctaves state) 
                             [ Pux.div [style { height     : "3%"
                                              , width      : "100%"
                                              , background : "#D4D4D4" } ] []
                             , Pux.div [ style { height     : "1%"
                                               , width      : "100%"
                                               , background : "#F4F4F4" } ] []
                             , Pux.div [ style { height     : "6%"
                                               , width      : "100%"
                                               , background : "#F4F4F4" } ] [ Pux.div [style { height      : "100%"
                                                                                             , width       : "33%"
                                                                                             , display     : "inline-block"}] []
                                                                            , Pux.div [style { height      : "100%"
                                                                                             , width       : "33%"
                                                                                             , display     : "inline-block" }] [ 
                                                                                                                               Pux.div [style { height      : "100%"
                                                                                                                                              , width       : "100%"
                                                                                                                                                }] buttons ]
                                                                            , Pux.div [style { height      : "100%"
                                                                                             , width       : "33%"
                                                                                             , display     : "inline-block"} ] [] ]
                             , Pux.div [ id_ "fooBar"
                                       , style { height     : "1%"
                                               , width      : "100%"
                                               , background : "#F4F4F4"} ] []
                             -- , Pux.div [ style { height     : "0.3%"
                             --                   , width      : "100%"
                             --                   , background : "#D4D4D4"] []
                             , Pux.div [ style { height     : "3%"
                                               , width      : "100%"
                                               , background : "white"} ] []

                             , Pux.div [ id_ "canvasDiv"
                                       , style { height     : "59%"
                                               , width      : "100%"
                                               , background : "white"
                                               , overflow   : "scroll"} ] [ canvas [ id_ "notationCanvas"
                                                                                   , style { overflow : "scroll"
                                                                                           , marginLeft : "1.4%"
                                                                                           , marginRight : "1%" }
                                                                                   , height "1000%"
                                                                                   , width "1240%" ] [text "HOI"] ]
                             , Pux.div [ style { height     : "6%"
                                               -- , fontSize   : "33"  
                                               , width      : "100%"
                                               , background : "#F4F4F4"
                                               } ] [ text $ "Currently selected note : " ++ show state.userMelody
                                                   , text $ "        " ++ show state.currentUserMelodyHead
                                                   , text $ "        " ++ show state.currentMidiKeyboardInput
                                                   , text $ "        " ++ show state.currentUIPianoSelection
                                                   , text $ "        " ++ show state.currentPlayBackNote
                                                   , text $ "        " ++ show state.currentPlayBackNoteIndex ]
                             , Pux.div [ style { height     : "20%"
                                               , width      : noteHelperDivSize state.noteHelperActivated ++ "%"
                                               , position   : "absolute"
                                               , right      : "0%"
                                               , border     : "2px solid #ddd"
                                               , bottom     : "21%"
                                               , overflow   : "scroll" } ] [ Pux.div [ style { height    : "100%"
                                                                                             , width      : noteHelperSize state.noteHelperActivated ++ "%"
                                                                                             , position   : "absolute"
                                                                                             , display    : "inline" } ] [Pux.canvas [ id_ "noteHelperCanvas"
                                                                                                                                     , style { background : "white"}
                                                                                                                                     , height "180%"
                                                                                                                                     , width "250%" ] [] ] 
                                                                          , Pux.div [ onClick $ const NoteHelperResize
                                                                                              , style { height     : "100%"
                                                                                                      , width      : "15px"
                                                                                                      , position   : "absolute"
                                                                                                      , background : "#a4a4a4"
                                                                                                      , display    : "inline" }] [] ]
                             ]
                            ) ]

buttons = [ Pux.div [ onClick $ const PlayButtonPressed
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "play.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
          , Pux.div [ onClick $ const PauseButtonPressed
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "pause.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
          , Pux.div [ onClick $ const ResetMelody
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "loop.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
          , Pux.div [ onClick $ const MuteButtonPressed
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "mute.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
            ]

noteHelperSize :: Boolean -> String
noteHelperSize isActivated = if isActivated then "100" else "0"

noteHelperDivSize :: Boolean -> String
noteHelperDivSize isActivated = if isActivated then "20" else "1"
          
octaveNumber :: Int
octaveNumber = 6

octaves :: Octave -> Array Octave
octaves n = range 1 (round (max (toNumber 1) (abs $ toNumber n)))
               
drawOctaves :: State -> Array (Html Action)
drawOctaves state = map (\x -> drawOctave x state.currentPlayBackNote [state.currentMidiKeyboardInput] state.currentUIPianoSelection) (octaves octaveNumber)
                    
drawOctave :: Octave  -> MidiNote -> Array MidiNote -> MidiNote -> (Html Action)
drawOctave oct notePlayed userNote selectedNote = Pux.div [] (drawKeys oct notePlayed userNote selectedNote)

drawKeys :: Octave -> MidiNote -> Array MidiNote -> MidiNote -> Array (Html Action)
drawKeys octave notePlayed userNote selectedNote  = map (\pos -> drawKey pos octave notePlayed userNote selectedNote) positions

drawKey :: PosRec -> Octave -> MidiNote -> Array MidiNote -> MidiNote -> Html Action                          
drawKey posRec octave notePlayed userNote selectedNote = (Pux.div [ onMouseDown (const (PianoKeyPressed posRec.note octave))
                                                                  -- , onMouseDown (const SetUserMelody)
                                                                  , styles posRec octave notePlayed userNote] [Pux.img [ src $ ((++) (getColor posRec) $ keyStatus posRec octave notePlayed userNote selectedNote) ++ keyShadow posRec octave
                                                                                                                       , style { height : "100%"
                                                                                                                               , width  : "100%"
                                                                                                                               , overflow : "hidden" } ] []  ])

getColor :: PosRec -> String
getColor posRec =  if posRec.isBlack then "black" else "white"

keyShadow :: PosRec -> Octave -> String
keyShadow posRec oct = if mod (toMidiNote posRec.note oct) 12 == 11 || mod (toMidiNote posRec.note oct) 12 == 4 then
                                      "BE.jpg"
                                    else
                                      ".jpg"

keyStatus :: PosRec -> Octave -> MidiNote -> Array MidiNote -> MidiNote -> String
keyStatus posRec octave notesPlayed userNotes selectedNote = if (toMidiNote posRec.note octave) == selectedNote then "Key_selected"
                                                             else if toMidiNote posRec.note octave == notesPlayed then "Key_red"
                                                             else if hasMidiNote (toMidiNote posRec.note octave) userNotes then "Key_grey"
                                                             else "Key"

styles :: PosRec -> Octave -> MidiNote -> Array MidiNote -> Attribute Action
styles posRec = if posRec.isBlack then styleBlack posRec else styleWhite posRec



type PosRec = { position :: Number
              , isBlack  :: Boolean
              , note     :: Note }

pos0 ::  PosRec
pos0 = { position   : 0.0 * whiteKeyWidth
       , isBlack    : false
       , note       : NoteC }
pos1 ::  PosRec
pos1 =  { position  : blackKeyOffset + (0.0 * whiteKeyWidth)
        , isBlack   : true
        , note      : NoteCis }
pos2 ::  PosRec
pos2 =  { position  : 1.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteD }
pos3 ::  PosRec
pos3 =  { position  : blackKeyOffset + (1.0 * whiteKeyWidth)
        , isBlack   : true
        , note      :  NoteDis }
pos4 ::  PosRec
pos4 =  { position  : 2.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteE }
pos5 ::  PosRec
pos5 =  { position  : 3.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteF }
pos6 ::  PosRec
pos6 =  { position  : blackKeyOffset + (3.0 * whiteKeyWidth)
        , isBlack   :  true
        , note      :  NoteFis }
pos7 ::  PosRec
pos7 =  { position  : 4.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteG }
pos8 ::  PosRec
pos8 =  { position  : blackKeyOffset + (4.0 * whiteKeyWidth)
        , isBlack   : true
        , note      : NoteGis }
pos9 ::  PosRec
pos9 =  { position  : 5.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteA }
pos10 ::  PosRec
pos10 =  { position : blackKeyOffset + (5.0 * whiteKeyWidth)
         , isBlack  : true
         , note     : NoteAis }
pos11 ::  PosRec
pos11 = { position  : 6.0 * whiteKeyWidth
        , isBlack   : false
        , note      : NoteB }

noteToInt :: Note -> Int
noteToInt NoteC   = 0
noteToInt NoteCis = 1
noteToInt NoteD   = 2
noteToInt NoteDis = 3
noteToInt NoteE   = 4
noteToInt NoteF   = 5
noteToInt NoteFis = 6
noteToInt NoteG   = 7
noteToInt NoteGis = 8
noteToInt NoteA   = 9
noteToInt NoteAis = 10
noteToInt NoteB   = 11

toMidiNote :: Note -> Octave -> Int
toMidiNote note oct = oct * 12 + noteToInt note

positions :: Array PosRec
positions = [pos0, pos2, pos4, pos5, pos7, pos9, pos11, pos1, pos3, pos6, pos8, pos10]

whiteKeyWidth :: Number
whiteKeyWidth = 100.0 / 7.0
blackKeyOffset :: Number
blackKeyOffset = (whiteKeyWidth * 0.7)

containerStyle :: Attribute Action
containerStyle =
  style { height     : "100%"
        , width      : "100%"
        , background : "white"
        , overflow   : "hidden" }
  
whiteKeyHeight :: Number
whiteKeyHeight = (6.7342 * (whiteKeyWidth / (abs $ toNumber octaveNumber)))

isCurrentNote :: PosRec -> Octave -> MidiNote -> Boolean
isCurrentNote posRec oct note = toMidiNote posRec.note oct == note

hasMidiNote :: MidiNote -> Array MidiNote -> Boolean
hasMidiNote note = foldl (\y midiNote -> if midiNote == note then true else y) false

styleWhite :: PosRec -> Int -> MidiNote -> Array MidiNote -> Attribute Action
styleWhite posRec octave notesPlayed userNotes =
  style { id_        : "whiteKey"
        , borderLeft     : "1px solid #444"
        , height     : show whiteKeyHeight ++ "%"
        , width      : show (whiteKeyWidth / (abs $ toNumber octaveNumber)) ++ "%"
        , left       : show (((100.0 / abs (toNumber octaveNumber)) * (toNumber octave - 1.0)) + (posRec.position / abs (toNumber octaveNumber))) ++ "%"
        , position   : "absolute"
        , bottom     : "1%" }

styleBlack :: PosRec -> Octave -> MidiNote -> Array MidiNote -> Attribute Action
styleBlack posRec octave notesPlayed userNotes =
  style { id_        : "blackKey"
        , height     : (show (0.7 * whiteKeyHeight)) ++ "%"
        , width      : show ((whiteKeyWidth * 0.6) / abs (toNumber octaveNumber)) ++ "%"
        , position   : "absolute"
        , left       : show (((100.0 / abs (toNumber octaveNumber)) * (toNumber octave - 1.0)) +  (posRec.position / abs (toNumber octaveNumber))) ++ "%"
        , bottom     : "6%" }

pianoStyle :: Attribute Action
pianoStyle =
  style
    { height     : "100%"
    , width      : "100%"
    , background : "black"
    , overflow   : "hidden"
    , position   : "relative"
    , float      : "left" }
