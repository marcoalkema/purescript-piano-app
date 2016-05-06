module App.Layout where

import App.UI as UI
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)
import Pux.Html.Attributes
import Pux.Html.Attributes (style)
import Pux.Html.Elements
import Control.Monad.Eff (Eff)
import HeartBeat
  
data Action
  = Child UI.Action
  | PageView Route

type State =
  { route :: Route
  , ui :: UI.State }

init :: State
init =
  { route: NotFound
  , ui: UI.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { ui = UI.update action state.ui }

view :: State -> Html Action
view state =
  div
    [ id_ "main", style { width : "100%"
                        , height : "700px" 
                        , overflow : "hidden"
                        , margin : "0px"} ] 
    [ case state.route of
        Home -> map Child $ UI.view state.ui
        NotFound -> App.NotFound.view state
    ]
    
