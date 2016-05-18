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

--kip

data Action = PianoKeyPressed Note Int | PianoKeyUp | PlayButtonPressed | NoteHelperResize | Piano Int | IncrementPlayBackIndex | UserNote Int

data Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type Octave = Int
type MidiNote = Int
type CurrentPlayBackNote = MidiNote
type CurrentUserNote = MidiNote

type State = { a                        :: Number
             , b                        :: Octave
             , currentPlayBackNotes     :: MidiNote
             , currentUserNotes         :: Array MidiNote
             , selectedNote             :: MidiNote 
             , noteHelperActivated      :: Boolean
             , playButtonPressed        :: Boolean
             , currentPlayBackNoteIndex :: Int
             , userNote                 :: Int}

update :: Action -> State -> State
update PlayButtonPressed state      = state { playButtonPressed = not state.playButtonPressed }
update NoteHelperResize state       = state { noteHelperActivated = not state.noteHelperActivated}
update PianoKeyUp state             = state { noteHelperActivated = state.noteHelperActivated}
update (PianoKeyPressed x y) state  = state { selectedNote = toMidiNote x y }
update (Piano n) state              = state { selectedNote = n }
update IncrementPlayBackIndex state = state { currentPlayBackNoteIndex = state.currentPlayBackNoteIndex + 1 }
update (UserNote n) state           = state { currentPlayBackNotes = n }

init :: State
init = { a                        : 0.0
       , b                        : 0
       , currentPlayBackNotes     : 0
       , currentUserNotes         : []
       , selectedNote             : 0
       , noteHelperActivated      : true
       , playButtonPressed        : false
       , currentPlayBackNoteIndex : 0
       , userNote                 : 0 }

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
                                                                                                                                                }] (map placeButton playBackButtons) ]
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
                                               , fontSize   : "33"  
                                               , width      : "100%"
                                               , background : "#F4F4F4"
                                               } ] [ text $ "Currently selected note : " ++ show state.currentPlayBackNoteIndex ]
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

noteHelperSize :: Boolean -> String
noteHelperSize isActivated = if isActivated then "100" else "0"

noteHelperDivSize :: Boolean -> String
noteHelperDivSize isActivated = if isActivated then "20" else "1"

playBackButtons :: Array String
playBackButtons = ["play.png", "pause.png", "loop.png", "mute.png"]

placeButton :: String -> Html Action
placeButton button = Pux.div [ onClick $ const PlayButtonPressed
                             , style { height     : "100%"
                                     , width      : "15%"
                                     , marginLeft : "10%"
                                     , display    : "inline"
                                     , float      : "left"
                                     , position   : "relative" } ] [ Pux.img [ src button
                                                                             , style { maxHeight : "100%"
                                                                                     , maxWidth  : "100%" 
                                                                                     } ] [] ]
octaveNumber :: Int
octaveNumber = 6

octaves :: Octave -> Array Octave
octaves n = range 1 (round (max (toNumber 1) (abs $ toNumber n)))
               
drawOctaves :: State -> Array (Html Action)
drawOctaves state = map (\x -> drawOctave x state.currentPlayBackNotes state.currentUserNotes state.selectedNote) (octaves octaveNumber)
                    
drawOctave :: Octave  -> MidiNote -> Array MidiNote -> MidiNote -> (Html Action)
drawOctave oct notePlayed userNote selectedNote = Pux.div [] (drawKeys oct notePlayed userNote selectedNote)

drawKeys :: Octave -> MidiNote -> Array MidiNote -> MidiNote -> Array (Html Action)
drawKeys octave notePlayed userNote selectedNote  = map (\pos -> drawKey pos octave notePlayed userNote selectedNote) positions

drawKey :: PosRec -> Octave -> MidiNote -> Array MidiNote -> MidiNote -> Html Action                          
drawKey posRec octave notePlayed userNote selectedNote = (Pux.div [ onMouseDown (const (PianoKeyPressed posRec.note octave) )
                                                                  , onMouseUp   (const PianoKeyUp)
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
