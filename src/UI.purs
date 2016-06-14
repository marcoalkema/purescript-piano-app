module App.UI where

import Prelude
import Prelude (map, (/))
import Math
import Math (max)
import Data.Int (round, toNumber)
import Data.Array
import Data.Array(head)
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
import MidiToVexFlow
import Data.Foreign
import ColorNotation
import VexMusic

data Action = PlayButtonPressed
            | PauseButtonPressed
            | StopButtonPressed
            | LoopButtonPressed
            | RecordButtonPressed
            | MetronomeButtonPressed
            | NoteButtonPressed
            | SettingsButtonPressed
            | NoteHelperResize
            | IncrementPlayBackIndex
            | ResetMelody
            | SetMidiKeyBoardInput MidiNote
            | PianoKeyPressed Note Octave
            | SetPlayBackNote MidiNote
            | SetMidiData (Array MidiNote)
            | SetMidiEvent (Array Foreign)
            | SetTicks Number
            | ResetPlayback
            | ScoreOkButtonPressed
            | TempoSliderChanged Int
            | SetLeftLocatorSlider Int
            | SetRightLocatorSlider Int

data Note = NoteC | NoteCis | NoteD | NoteDis | NoteE | NoteF | NoteFis | NoteG | NoteGis | NoteA | NoteAis | NoteB

type Octave              = Int
type MidiNote            = Int
type CurrentPlayBackNote = MidiNote
type CurrentUserNote     = MidiNote


-- TODO: Separate states for different domains --> Create new / domain-specific states!!!
--SEPARATION OF CONCERNS!!!
type State = { currentMidiKeyboardInput :: MidiNote
             , currentUIPianoSelection  :: MidiNote
             , currentPlayBackNote      :: MidiNote
             , currentPlayBackNoteIndex :: Int

             , currentSelectedNote      :: MidiNote
               
             , currentPlayBackMelody    :: Array MidiNote
             , userMelody               :: Array MidiNote
             , currentUserMelodyHead    :: MidiNote
             , currentNoteHelperNote    :: MidiNote
             , userNotes                :: Array MidiNote
             , lastNote                 :: Array MidiNote

             , midiData                 :: Array MidiNote
             , ticks                    :: Number
             , midiEvents               :: VexFlowResult
             , colorNotation            :: NotationHasColor

             , scoreWindowActivated     :: Boolean
               
             , noteHelperActivated      :: Boolean
             , playButtonPressed        :: Boolean
             , pauseButtonPressed       :: Boolean
             , stopButtonPressed        :: Boolean
             , recordButtonPressed      :: Boolean
             , metronomeButtonPressed   :: Boolean
             , loopButtonPressed        :: Boolean
             , noteButtonPressed        :: Boolean
             , settingsButtonPressed    :: Boolean
             , tempoSliderValue         :: Int
             , leftLocator              :: Int
             , rightLocator             :: Int}

update :: Action -> State -> State

update (SetMidiKeyBoardInput n) state = state { currentMidiKeyboardInput = n
                                              , currentSelectedNote      = n - 12
                                              , userNotes                = if state.recordButtonPressed then
                                                                             snoc state.userNotes n
                                                                           else
                                                                             []
                                              , currentPlayBackNoteIndex = if state.recordButtonPressed then
                                                                             incIndex
                                                                           else
                                                                             state.currentPlayBackNoteIndex
                                              , userMelody               = if state.recordButtonPressed then
                                                                             newMelody
                                                                           else
                                                                             state.currentPlayBackMelody
                                              , currentUserMelodyHead    = fromMaybe 0 $ head newMelody
                                              , scoreWindowActivated     = checkLastNote n
                                              , recordButtonPressed      = if checkLastNote n then
                                                                             false
                                                                           else
                                                                             true }
    where
      checkLastNote n = if (state.currentPlayBackNoteIndex == length state.midiData - 1) && ((Just n) == (currentNote)) then
                          true
                        else
                          false
      currentNote :: Maybe Int
      currentNote = head state.userMelody
      newMelody = matchUserInput state.userMelody
      incIndex = if currentNote == Nothing then
                   0
                 else if (Just n) == currentNote then
                        state.currentPlayBackNoteIndex + 1
                      else
                        state.currentPlayBackNoteIndex
      matchUserInput :: Array MidiNote -> Array MidiNote
      matchUserInput playBackNotes = if currentNote == Nothing then
                                       state.currentPlayBackMelody
                                     else if (Just n) == currentNote then
                                            fromMaybe [] $ tail playBackNotes
                                          else
                                            playBackNotes
                                              
update (PianoKeyPressed x y) state    = state { currentUIPianoSelection  = toMidiNote x y
                                              , currentSelectedNote      = (toMidiNote x y)}
update (SetPlayBackNote n) state      = state { currentPlayBackNote      = n}
update IncrementPlayBackIndex state   = state { currentPlayBackNoteIndex = state.currentPlayBackNoteIndex + 1 }

update ResetMelody state              = state { userMelody = state.userMelody }

update PlayButtonPressed state        = state { playButtonPressed        = not state.playButtonPressed }
update StopButtonPressed state        = state { stopButtonPressed        = not state.playButtonPressed
                                              , currentPlayBackNoteIndex = -1
                                              , recordButtonPressed      = false
                                              , playButtonPressed        = true }
update PauseButtonPressed state       = state { pauseButtonPressed       = not state.pauseButtonPressed }
update LoopButtonPressed state        = state { loopButtonPressed        = not state.loopButtonPressed  }
update NoteButtonPressed state        = state { noteButtonPressed        = not state.noteButtonPressed  }
update RecordButtonPressed state      = state { recordButtonPressed      = not state.recordButtonPressed
                                              , userMelody               = state.currentPlayBackMelody
                                              , currentPlayBackNoteIndex = if state.recordButtonPressed then
                                                                             -1
                                                                           else
                                                                             0
                                              , playButtonPressed        = true
                                              , userNotes                = [] }
update MetronomeButtonPressed state   = state { metronomeButtonPressed   = not state.metronomeButtonPressed }
update SettingsButtonPressed state    = state { settingsButtonPressed    = not state.settingsButtonPressed }
update ScoreOkButtonPressed state     = state { scoreWindowActivated     = false }
update (TempoSliderChanged n) state   = state { tempoSliderValue         = n }

update NoteHelperResize state         = state { noteHelperActivated      = not state.noteHelperActivated }

update (SetMidiData d) state          = state { midiData              = d
                                              , currentPlayBackMelody = d
                                              , userMelody            = d
                                              , lastNote              = reverse <<< take 2 $ reverse d }
update (SetTicks d) state             = state { ticks = d }
update (SetMidiEvent d) state         = if null d then
                                          state { midiEvents    = initEvent
                                                , colorNotation = [[[]]] }
                                        else
                                          state { midiEvents    = midi
                                                , colorNotation = setInitColor midi.vexFlowNotes}
  where
    midi = renderMidiPure d state.ticks
update ResetPlayback state            = state { currentPlayBackNoteIndex = -1
                                              , playButtonPressed        = true}

update (SetLeftLocatorSlider n) state = state { leftLocator = n }
update (SetRightLocatorSlider n) state = state { rightLocator = n }


init :: State
init = { currentMidiKeyboardInput : 60
       , currentUIPianoSelection  : 0
       , currentPlayBackNote      : 60
       , currentPlayBackNoteIndex : -1
       , currentSelectedNote      : 0

       , currentPlayBackMelody    : []
       , userMelody               : []
       , currentUserMelodyHead    : 0
       , currentNoteHelperNote    : 60
       , userNotes                : []
       , lastNote                 : []

       , midiData                 : []
       , ticks                    : 480.0
       , midiEvents               : initEvent
       , colorNotation            : [[[]]]

       , scoreWindowActivated     : false
         
       , noteHelperActivated      : true
       , playButtonPressed        : true
       , pauseButtonPressed       : false
       , stopButtonPressed        : false
       , recordButtonPressed      : false
       , metronomeButtonPressed   : false
       , loopButtonPressed        : true
       , noteButtonPressed        : false
       , settingsButtonPressed    : true
         
       , tempoSliderValue         : 120
       , leftLocator              : 1
       , rightLocator             : 1}


-- TODO: FIX: initEvent gives runtime error in VexFlow: Voice does not have enough notes.
initEvent = { vexFlowNotes : [[[{ pitch    : ["c/4"]
                              , duration :  "1"    }]]]
          , vexNotes     : [[[]]]
          , indexedTies  : [[]]
          , indexedBeams : [[[]]]
          , numerator    : 1 }
          
setMelody :: State -> State
setMelody state = state { userMelody            = newMelody 
                        , currentUserMelodyHead = fromMaybe 0 $ head newMelody }
  where
    newMelody = matchUserInput state.currentMidiKeyboardInput state.userMelody

matchUserInput :: MidiNote -> Array MidiNote -> Array MidiNote
matchUserInput userNote playBackNotes = if currentNote == Nothing then
                                          []
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
                                                                                             , width       : "25%"
                                                                                             , display     : "inline-block"}] []
                                                                            , Pux.div [style { height      : "100%"
                                                                                             , width       : "25%"
                                                                                             , display     : "inline-block" }] [ 
                                                                                                                               Pux.div [style { height      : "100%"
                                                                                                                                              , width       : "100%"
                                                                                                                                                }] (buttons state) ]
                                                                            , Pux.div [style { height      : "100%"
                                                                                             , width       : "25%"
                                                                                             , display     : "inline-block"} ] [ Pux.div [ id_ "Record_button"
                                                                                                                                         , onClick $ const RecordButtonPressed
                                                                                                                                         , style { height     : "100%"
                                                                                                                                                 , width      : "15%"
                                                                                                                                                 , marginLeft : "10%"
                                                                                                                                                 , display    : "inline"
                                                                                                                                                 , float      : "left"
                                                                                                                                                 , position   : "relative" } ] [ Pux.img [ src $ recordButtonPressed state.recordButtonPressed
                                                                                                                                                                                         , style { maxHeight : "100%"
                                                                                                                                                                                                 , maxWidth  : "100%" 
                                                                                                                                                                                                 } ] [] ]] 
                                                                            , Pux.div [style { height      : "100%"
                                                                                             , width       : "25%"
                                                                                             , display     : "inline-block"} ] [ Pux.div [ id_ "Metronome"
                                                                                                                                         , onClick $ const MetronomeButtonPressed
                                                                                                                                         , style { height     : "100%"
                                                                                                                                                 , width      : "15%"
                                                                                                                                                 , marginRight : "10%"
                                                                                                                                                 , display    : "inline"
                                                                                                                                                 , float      : "left"
                                                                                                                                                 , position   : "relative" } ] [ Pux.img [ src $ metronomeButtonPressed state.metronomeButtonPressed
                                                                                                                                                                                         , style { maxHeight : "100%"
                                                                                                                                                                                                 , maxWidth  : "100%" 
                                                                                                                                                                                                 } ] [] ]
                                                                                                                               , Pux.div [ id_ "Loop_button"
                                                                                                                                         , onClick $ const LoopButtonPressed
                                                                                                                                         , style { height     : "100%"
                                                                                                                                                 , width      : "15%"
                                                                                                                                                 , marginRight : "10%"
                                                                                                                                                 , display    : "inline"
                                                                                                                                                 , float      : "left"
                                                                                                                                                 , position   : "relative" } ] [ Pux.img [ src if state.loopButtonPressed then
                                                                                                                                                                                                 "loopButton.png"
                                                                                                                                                                                                 else
                                                                                                                                                                                                 "loopButtonPressed.png"                                                                                                                                                                       
                                                                                                                                                                                         , style { maxHeight : "100%"
                                                                                                                                                                                                 , maxWidth  : "100%" 
                                                                                                                                                                                                 } ] [] ]
                                                                                                                               , Pux.div [ id_ "Note_button"
                                                                                                                                         -- , onClick $ const NoteButtonPressed
                                                                                                                                         , style { height     : "100%"
                                                                                                                                                 , width      : "15%"
                                                                                                                                                 , marginRight : "10%"
                                                                                                                                                 , display    : "inline"
                                                                                                                                                 , float      : "left"
                                                                                                                                                 , position   : "relative" } ] [ Pux.img [ src "note.png"
                                                                                                                                                                                         , style { maxHeight : "100%"
                                                                                                                                                                                                 , maxWidth  : "100%" 
                                                                                                                                                                                                 } ] [] ]
                                                                                                                               , Pux.div [ id_ "Settings_button"
                                                                                                                                         , onClick $ const SettingsButtonPressed
                                                                                                                                         , style { height     : "100%"
                                                                                                                                                 , width      : "15%"
                                                                                                                                                 , marginRight : "10%"
                                                                                                                                                 , display    : "inline"
                                                                                                                                                 , float      : "left"
                                                                                                                                                 , position   : "relative" } ] [ Pux.img [ src if state.settingsButtonPressed then
                                                                                                                                                                                                 "settingsButton.png"
                                                                                                                                                                                                 else
                                                                                                                                                                                                 "settingsButtonPressed.png"
                                                                                                                                                                                         , style { maxHeight : "100%"
                                                                                                                                                                                                 , maxWidth  : "100%" 
                                                                                                                                                                                                 } ] [] ]] ]
                             , Pux.div [ id_ "fooBar"
                                       , style { height     : "1%"
                                               , width      : "100%"
                                               , background : "#F4F4F4"} ] []
                             , Pux.div [ style { height     : "3%"
                                               , width      : "100%"
                                               , background : "white"} ] []

                             , Pux.div [ id_ "canvasDiv"
                                       , style { height     : "6b5%"
                                               , width      : "100%"
                                               , background : "white"
                                               , overflow   : "scroll"} ] [ canvas [ id_ "notationCanvas"
                                                                                   , style { overflow : "scroll"
                                                                                           , marginLeft : "1.4%"
                                                                                           , marginRight : "1%" }
                                                                                   , height "1000%"
                                                                                   , width "1240%" ] [] ]
                             -- , Pux.div [ style { height     : "6%"
                             --                   , width      : "100%"
                             --                   , background : "#F4F4F4"
                             --                   } ] [ text $ "Currently selected note : " ++ show state.currentPlayBackMelody
                             --                       , text $ "        " ++ show state.currentUserMelodyHead
                             --                       , text $ "        " ++ show state.userMelody                                                     
                             --                       , text $ "        " ++ show state.ticks
                             --                       , text $ "        " ++ show state.currentMidiKeyboardInput
                             --                       , text $ "        " ++ show state.currentUIPianoSelection
                             --                       , text $ "        " ++ show state.currentPlayBackNote
                             --                       , text $ "        " ++ show state.currentPlayBackNoteIndex
                             --                       , text $ "        " ++ show state.lastNote
                             --                       , text $ "        " ++ show state.tempoSliderValue
                             --                       , text $ "        " ++ show state.userNotes ]
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
                             , Pux.div [style { height     : resizeWindow' state.scoreWindowActivated 40
                                              , width      : resizeWindow' state.scoreWindowActivated 40
                                              , top        : "25%"
                                              , left       : "30%"
                                              , position   : "absolute"
                                              , background : "#FFFFFF"
                                              , fontSize   : "36px" }] [ Pux.div [style { height  : "70%"
                                                                                        , width   : "100%"} ] [ Pux.div [style { height : "20%"
                                                                                                                               , width  : "70%"
                                                                                                                               , marginTop : "2%"
                                                                                                                               , marginLeft   : "25%"
                                                                                                                            }] [text $ displayScore state]]
                                                                       , Pux.div [style { height  : "30%"
                                                                                        , width   : "100%" }] [ Pux.div [style { height : "100%"
                                                                                                                                    -- , width  : "33,3%"
                                                                                                                                    , borderLeft   : "33.3%"
                                                                                                                                    , display : "inline"} ] [ Pux.div [ style { height : "100%"
                                                                                                                                                                               , width  : "100%"} ] [ Pux.img [ onClick $ const ScoreOkButtonPressed
                                                                                                                                                                                                              , src "okButton.png"
                                                                                                                                                                                                              , style { height : "50%"
                                                                                                                                                                                                                      , top      : "30%"
                                                                                                                                                                                                                      , marginLeft : "39%"
                                                                                                                                                                                                                      , position : "relative"}] [] ]]]
                               ]
                             , Pux.div [style { height     : "55%"
                                              , width      : "55%"
                                              , top        : "15%"
                                              , left       : (if state.settingsButtonPressed then
                                                               "100%"
                                                               else
                                                               "22.5%")
                                              , position   : "absolute"
                                              , background : "#FFFFFF"
                                              , border     : "3px solid #DDD"
                                              , fontFamily : "Amiri"
                                              , fontStyle  : "serif"
                                              , fontSize   : "30px" }] [ Pux.div [ style { height : "33.3%"
                                                                                         , width  : "100%"
                                                                                         , background : "white"} ] [ Pux.div [ style { marginTop   : "3.5%"
                                                                                                                                      , marginLeft  : "30%"
                                                                                                                                      , height      : "30%"
                                                                                                                                      , textAlign   : "center"
                                                                                                                                      , width       : "40%"
                                                                                                                                      , color       : "#FAFAFA"
                                                                                                                                      , background  : "#679292"} ] [ text "LOOP"
                                                                                                                                                                   , Pux.div [style { height : "50%" }] [ Pux.Html.Elements.input [ type_ "range"
                                                                                                                                                                                                                                  , id_ "leftLocator"
                                                                                                                                                                                                                                  , Pux.Html.Attributes.min "1"
                                                                                                                                                                                                                                  , Pux.Html.Attributes.max "20"
                                                                                                                                                                                                                                  , Pux.Html.Attributes.step "1"
                                                                                                                                                                                                                                  , Pux.Html.Attributes.defaultValue "1"

                                                                                                                                                                                                                                  ] []]
                                                                                                                                                                   , Pux.div [style { height : "50%" }] [ Pux.Html.Elements.input [ type_ "range"
                                                                                                                                                                                                                                , id_ "rightLocator"
                                                                                                                                                                                                                                , Pux.Html.Attributes.min "1"
                                                                                                                                                                                                                                , Pux.Html.Attributes.max "20"
                                                                                                                                                                                                                                , Pux.Html.Attributes.step "1"
                                                                                                                                                                                                                                , Pux.Html.Attributes.defaultValue "1"
                                                                                                                                                                                                                                , style { marginTop : "18%"}
                                                                                                                                                                                                                                ] []
                                                                                                                                                                                                        ]
                                                                                                                     ]
                                                                                                                    , Pux.div [ style { width : "30%"
                                                                                                                                      , height : "76%"
                                                                                                                                      , marginLeft : "70%"}] [ Pux.div [style { height : "47%"
                                                                                                                                                                              , marginTop : "3%"
                                                                                                                                                                              , marginLeft : "5%"
                                                                                                                                                                              , width  : "100%"}] [text $ " START : " ++ (show state.leftLocator)]
                                                                                                                                                             , Pux.div [style { height : "47%"
                                                                                                                                                                              , marginTop : "2%"
                                                                                                                                                                              , marginLeft : "5%"
                                                                                                                                                                              , width  : "100%"}] [text $ " END   : " ++ (show state.rightLocator)]
                                                                                                                                                             ]
                                                                                                                    ]
                                                                       , Pux.div [ style { height : "33.3%"
                                                                                         , width  : "100%"} ] [ Pux.div [ style { marginTop   : "1.3%"
                                                                                                                                , marginLeft  : "30%"
                                                                                                                                , height      : "30%"
                                                                                                                                , textAlign   : "center"
                                                                                                                                , width       : "40%"
                                                                                                                                , color       : "#FAFAFA"
                                                                                                                                , background  : "#679292"} ] [ text "METRONOME"
                                                                                                                                                             , Pux.div [style { height    : "70%"
                                                                                                                                                                              , marginTop : "5%"
                                                                                                                                                                              , color  : "#333"}] [ text $ show state.tempoSliderValue ]
                                                                                                                                                             , Pux.div [style { height : "50%" }] [ Pux.Html.Elements.input [ type_ "range"
                                                                                                                                                                                                                    , id_ "metronomeSlider"
                                                                                                                                                                                                                    , Pux.Html.Attributes.min "20"
                                                                                                                                                                                                                    , Pux.Html.Attributes.max "200"
                                                                                                                                                                                                                    , Pux.Html.Attributes.step "1"
                                                                                                                                                                                                                    , Pux.Html.Attributes.defaultValue "120"

                                                                                                                                                                                                                    ] []
                                                                                                                                                                                          ]]]
                                                                       , Pux.div [ style { height : "33.3%"
                                                                                         , width  : "100%"} ] [Pux.div [ style { marginTop   : "1.3%"
                                                                                                                                      , marginLeft  : "30%"
                                                                                                                                      , height      : "30%"
                                                                                                                                      , textAlign   : "center"
                                                                                                                                      , width       : "40%"
                                                                                                                                      , color       : "#FAFAFA"
                                                                                                                                      , background  : "#679292"} ] [ text "SETTINGS"]]
                                                                       ]
                               
                             ]
                             
                             
                            )]

buttons state = [ Pux.div [ id_ "Play_button"
                          , onClick $ const PlayButtonPressed
                          , style { height     : "100%"
                                  , width      : "15%"
                                  , marginLeft : "10%"
                                  , display    : "inline"
                                  , float      : "left"
                                  , position   : "relative" } ] [ Pux.img [ src if state.playButtonPressed then
                                                                                  "playButton.png"
                                                                                else
                                                                                  "playButtonPressed.png"
                                                                          , style { maxHeight : "100%"
                                                                                  , maxWidth  : "100%" 
                                                                                  } ] [] ]
          , Pux.div [ id_ "Pause_button"
                    , onClick $ const PauseButtonPressed
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "pause.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
          , Pux.div [ id_ "Stop_button"
                    , onClick $ const StopButtonPressed
                    , style { height     : "100%"
                            , width      : "15%"
                            , marginLeft : "10%"
                            , display    : "inline"
                            , float      : "left"
                            , position   : "relative" } ] [ Pux.img [ src "stop.png"
                                                                    , style { maxHeight : "100%"
                                                                            , maxWidth  : "100%" 
                                                                            } ] [] ]
          ]
          
metronomeButtonPressed b = if b then
                             "metronome_pressed.png"
                           else
                             "metronome.png"

recordButtonPressed b = if b then
                             "recordButtonPressed.png"
                           else
                             "recordButton.png"

resizeWindow b = if b then
                   "5%"
                 else
                   "0%"

resizeWindow' :: Boolean -> Int -> String
resizeWindow' b size = if b then
                         (++) (show size) "%"
                       else
                         "0%"

displayScore :: State -> String
displayScore s = if s.scoreWindowActivated then
                       "Your score is: " ++ (show $ round $ userScore s) ++ "%"
                     else
                       ""


userScore :: State -> Number
userScore state = if length state.userNotes == 0 then
                    0.0
                  else
                    (toNumber $ length state.currentPlayBackMelody) / (toNumber $ length state.userNotes) * 100.0

noteHelperSize :: Boolean -> String
noteHelperSize isActivated = if isActivated then "100" else "0"

noteHelperDivSize :: Boolean -> String
noteHelperDivSize isActivated = if isActivated then "20" else "1"
          
octaveNumber :: Int
octaveNumber = 6

octaves :: Octave -> Array Octave
octaves = range 1 <<< round <<< max 1.0 <<< abs <<< toNumber
               
drawOctaves :: State -> Array (Html Action)
drawOctaves state = map (\x -> drawOctave x (checkRecordEnabled state) [state.currentMidiKeyboardInput] state.currentSelectedNote) (octaves octaveNumber)

checkRecordEnabled :: State -> MidiNote
checkRecordEnabled s = if s.recordButtonPressed then
                         fromMaybe 0 $ Data.Array.head s.userMelody
                         else
                         s.currentPlayBackNote
                    
drawOctave :: Octave  -> MidiNote -> Array MidiNote -> MidiNote -> (Html Action)
drawOctave oct notePlayed userNote = Pux.div [] <<< drawKeys oct notePlayed userNote 

drawKeys :: Octave -> MidiNote -> Array MidiNote -> MidiNote -> Array (Html Action)
drawKeys octave notePlayed userNote selectedNote  = map (\pos -> drawKey pos octave notePlayed userNote selectedNote) positions

drawKey :: PosRec -> Octave -> MidiNote -> Array MidiNote -> MidiNote -> Html Action                          
drawKey posRec octave notePlayed userNote selectedNote = (Pux.div [ onMouseDown (const (PianoKeyPressed posRec.note octave))
                                                                  , id_ $ "pianoKey" ++ (show $ (toMidiNote posRec.note octave) - 12)
                                                                  , styles posRec octave notePlayed userNote] [Pux.img [ src $ ((++) (getColor posRec) $ keyStatus posRec octave notePlayed userNote selectedNote) ++ keyShadow posRec octave
                                                                                                                       , style { height : "100%"
                                                                                                                               , width  : "100%"
                                                                                                                               , overflow : "hidden" } ] []  ])
setPianoId :: Octave -> MidiNote -> Int
setPianoId o = (+) (12 * o)

getColor :: PosRec -> String
getColor posRec =  if posRec.isBlack then "black" else "white"

keyShadow :: PosRec -> Octave -> String
keyShadow posRec oct = if mod (toMidiNote posRec.note oct) 12 == 11 || mod (toMidiNote posRec.note oct) 12 == 4 then
                                      "BE.jpg"
                                    else
                                      ".jpg"

keyStatus :: PosRec -> Octave -> MidiNote -> Array MidiNote -> MidiNote -> String
keyStatus posRec octave notesPlayed userNotes selectedNote = if (toMidiNote posRec.note octave) == selectedNote then "Key_selected"
                                                             else if toMidiNote posRec.note octave == notesPlayed - 12 then "Key_red"
                                                             -- else if hasMidiNote (toMidiNote posRec.note octave) userNotes then "Key_grey"
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

-- How to get rid of parentheses??? / function comp: multiple argumentes
toMidiNote :: Note -> Octave -> Int
toMidiNote note = (+) (noteToInt note) <<< (*) 12

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
hasMidiNote note = foldl (\b midiNote -> if midiNote == note then true else b) false

styleWhite :: PosRec -> Int -> MidiNote -> Array MidiNote -> Attribute Action
styleWhite posRec octave notesPlayed userNotes =
  style { id_        : "whiteKey"
        , borderLeft : "1px solid #444"
        , height     : show whiteKeyHeight ++ "%"
        , width      : show ((/) whiteKeyWidth $ abs $ toNumber octaveNumber) ++ "%"
        , left       : show ((((/) 100.0 $ abs $ toNumber octaveNumber) * (toNumber octave - 1.0)) + (posRec.position / abs (toNumber octaveNumber))) ++ "%"
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
