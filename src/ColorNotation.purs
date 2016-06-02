module ColorNotation where

import Prelude
import Data.Array
import Data.Maybe
import Data.Foldable
import VexMusic

type NotationHasColor = Array (Array (Array Boolean))
type BarHasColor      = Array (Array Boolean)
type VoiceHasColor    = Array Boolean
type Index            = Int

type HasColor = { acc         :: Int
                , barHasColor :: BarHasColor }

hasColorInit = { acc         : 0
               , barHasColor : [] }

setHasColor :: Index -> NotationHasColor -> NotationHasColor
setHasColor i xs = getHasColorFromRecord $ setBoolean i xs

getHasColorFromRecord :: Array HasColor -> NotationHasColor
getHasColorFromRecord xs = map (_.barHasColor) $ getTail xs

setBoolean :: Index -> NotationHasColor -> Array HasColor
setBoolean i xs = foldl (\b x -> Data.Array.snoc b $ compareToIndex i ((getAcc b).acc) x) [hasColorInit] xs

getTail :: Array HasColor -> Array HasColor
getTail xs = fromMaybe [] $ Data.Array.tail xs

getAcc :: Array HasColor -> HasColor
getAcc r = fromMaybe hasColorInit (Data.Array.last r)

compareToIndex :: Index -> Int -> BarHasColor -> HasColor
compareToIndex i n bar = if n < (i - currentArrayLength) then
                          { acc         : (+) n currentArrayLength
                          , barHasColor : bar }
                        else
                          { acc         : (+) n currentArrayLength
                          , barHasColor : setToTrue }
  where
    currentArrayIndex     = i - n
    currentArrayLength    = length $ concat bar
    setElementToTrue      :: Int -> VoiceHasColor -> Maybe (VoiceHasColor)
    setElementToTrue i v  = updateAt i true v
    setToTrue             :: BarHasColor
    setToTrue             = map (\voice -> fromMaybe (Data.Array.concat bar) (setElementToTrue currentArrayIndex voice)) bar

    --Lenses
setInitColor :: VexFlowMusic -> NotationHasColor
setInitColor =  mapVoices $ const false
  where
    mapVoices = map <<< map <<< map
