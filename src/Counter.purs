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


data Action = Action1 | Action2 | PianoKeyPressed Note Int | PianoKeyUp | PlayBackNote Int

data Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type State = { a                    :: Number
             , b                    :: Octave
             , currentPlayBackNotes :: Array MidiNote
             , currentUserNotes     :: Array MidiNote }

update :: Action -> State -> State
update Action1 state = { a                    : 0.0
                         , b                    : 0 
                         , currentPlayBackNotes : [60, 64, 67]
                         , currentUserNotes     : [38, 42, 65]}
update Action2 state = { a                    : 0.0
                         , b                    : 10
                         , currentPlayBackNotes : [62, 66, 69]
                         , currentUserNotes     : [40, 44, 67]}
update PianoKeyUp state = { a                    : 0.0
                          , b                    : 1337
                          , currentPlayBackNotes : [62, 66, 69]
                          , currentUserNotes     : [40, 44, 67]}
update (PianoKeyPressed x y) state = {  a                    : 0.0
                                      , b                    : toMidiNote x y
                                      , currentPlayBackNotes : [62, 66, 69]
                                      , currentUserNotes     : [40, 44, 67]}
-- update (PlayBackNote x) state = state { currentPlayBackNotes = (cons x state.currentPlayBackNotes)}

updateCurrentPlaybackNotes :: State -> Int -> State
updateCurrentPlaybackNotes state n = state { currentPlayBackNotes = cons n state.currentPlayBackNotes }

updateCurrentUserNotes :: State -> Int -> State
updateCurrentUserNotes state n = state { currentUserNotes = cons n state.currentUserNotes }
                                 
type Octave = Int

type MidiNote = Int
type CurrentPlayBackNote = MidiNote
type CurrentUserNote = MidiNote

currentNotePlayed :: CurrentPlayBackNote
currentNotePlayed = 51

currentUserNote :: CurrentUserNote
currentUserNote = 60

init :: State
init = { a : 0.0
       , b : 0
       , currentPlayBackNotes : [60, 64, 67]
       , currentUserNotes     : [38, 42, 65] }

view :: State -> Html Action
view state = do
  Pux.div
    [ style { height : "100%"
            , width : "100%"
            , overflow : "hidden" }
    , id_ "main"
    ]
    [ Pux.div [ pianoStyle
              , id_ "foo" ] (foldr (\y x -> cons y x) (drawOctaves state) 
                             [ Pux.div [ style { height     : "15%"
                                               , width      : "100%"
                                               , background : "#F4F4F4" } ] [ Pux.div [ onClick (const Action2), style { height     : "98%"
                                                                                                                         , width      : "8%"
                                                                                                                         , display    : "inline-block"
                                                                                                                         , marginLeft : "13.6%"
                                                                                                                         , background : "yellow" } ] []
                                                                            , Pux.div [ style { height     : "98%"
                                                                                              , width      : "8%"
                                                                                              , marginLeft : "13.6%"
                                                                                              , display    : "inline-block"
                                                                                              , left       : "20%"
                                                                                              , background : "blue" } ] []
                                                                            , Pux.div [ onClick (const Action1), style { height     : "98%"
                                                                                                                         , width      : "8%"
                                                                                                                         , marginLeft : "13.6%"
                                                                                                                         , display    : "inline-block"
                                                                                                                         , left       : "20%"
                                                                                                                         , background : "green" } ] []
                                                                                       , Pux.div [ style { height       : "98%"
                                                                                                           , width      : "8%"
                                                                                                           , marginLeft : "13.6%"
                                                                                                           , display    : "inline-block"
                                                                                                           , left       : "20%"
                                                                                                           , background : "red" } ] []
                                                                                       ]
                             , Pux.div [ id_ "canvasDiv"
                                       , style { height     : "40%"
                                               , width      : "100%"
                                               , background : "white" } ] [ canvas [id_ "notationCanvas", style { height : "100%"
                                                                                                                , width  : "100% " }] []
                                                                          , text $ show state.b
                                                                          ]
                             , Pux.div [ style { height     : "15%"
                                               , width      : "100%"
                                               , background : "#F4F4F4" } ] []
                             ]
                            ) ]

octaveNumber :: Int
octaveNumber = 6
               
drawOctaves :: State -> Array (Html Action)
drawOctaves state = map (\x -> drawOctave x state.currentPlayBackNotes state.currentUserNotes) (octaves octaveNumber)
                    
octaves :: Octave -> Array Octave
octaves n = range 1 (round (max (toNumber 1) (abs $ toNumber n)))
            
drawOctave :: Octave  -> Array MidiNote -> Array MidiNote -> (Html Action)
drawOctave oct notePlayed userNote = Pux.div [] (drawKeys oct notePlayed userNote)

drawKeys :: Octave -> Array MidiNote -> Array MidiNote -> Array (Html Action)
drawKeys octave notePlayed userNote = map (\pos -> drawKey pos octave notePlayed userNote) positions

drawKey :: PosRec -> Octave -> Array MidiNote -> Array MidiNote -> Html Action                          
drawKey posRec octave notePlayed userNote  = (Pux.div [ onMouseDown (const (PianoKeyPressed posRec.note octave) )
                                                      , onMouseUp (const PianoKeyUp)
                                                      , styles posRec octave notePlayed userNote] [])

styles :: PosRec -> Octave -> Array MidiNote -> Array MidiNote -> Attribute Action
styles posRec  = if posRec.isBlack then styleBlack posRec else styleWhite posRec 

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
        , isBlack      : false
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

styleWhite :: PosRec -> Int -> Array MidiNote -> Array MidiNote -> Attribute Action
styleWhite posRec octave notesPlayed userNotes =
  style { id_        : "whiteKey"
        , background : if hasMidiNote (toMidiNote posRec.note octave) notesPlayed then "green"
                       else if hasMidiNote (toMidiNote posRec.note octave) userNotes then "red"
                       else "white"
        , border     : "2px solid #aaa"
        , height     : show whiteKeyHeight ++ "%"
        , width      : show (whiteKeyWidth / (abs $ toNumber octaveNumber)) ++ "%"
        , left       : show (((100.0 / abs (toNumber octaveNumber)) * (toNumber octave - 1.0)) + (posRec.position / abs (toNumber octaveNumber))) ++ "%"
        , position   : "absolute"
        , bottom     : "10%" }

styleBlack :: PosRec -> Octave -> Array MidiNote -> Array MidiNote -> Attribute Action
styleBlack posRec octave notesPlayed userNotes =
  style { id_        : "blackKey"
        , background : if hasMidiNote (toMidiNote posRec.note octave) notesPlayed then "green"
                       else if hasMidiNote (toMidiNote posRec.note octave) userNotes then "red"
                       else "black"
        , color      : "red"
        , height     : (show (0.7 * whiteKeyHeight)) ++ "%"
        , width      : show ((whiteKeyWidth * 0.6) / abs (toNumber octaveNumber)) ++ "%"
        , position   : "absolute"
        , left       : show (((100.0 / abs (toNumber octaveNumber)) * (toNumber octave - 1.0)) +  (posRec.position / abs (toNumber octaveNumber))) ++ "%"
        , bottom     : "15.2%" }

pianoStyle    :: Attribute Action
pianoStyle =
  style
    { height     : "100%"
    , width      : "100%"
    , background : "black"
    , overflow   : "hidden"
    , position   : "relative"
    , fontSize   : "72px"
    , float      : "left" }

-- buttonStyle :: Attribute
-- buttonStyle =
--   style
--     [ ("height", "104px")
--     , ("width", "60px")
--     , ("background-color", "#ddd")
--     , ("border", "2px solid #aaa")
--     , ("top", "200px")
--     , ("position", "relative")
--     , ("font-size", "72px")
--     , ("float", "left")
--     , ("left", "10px")
--     , ("z-index", "4")  
--     ]

-- pianoStyle' :: Attribute
-- pianoStyle' =
--   style
--     [ ("height", "360px")
--     , ("width", "98%")
--     , ("border", "2px solid #aaa")
--     , ("top", "210px")
--     , ("position", "relative")
--     , ("font-size", "72px")
--     , ("float", "left")
--     , ("left", "10px")
--     , ("z-index", "4")  
--     ]                  
