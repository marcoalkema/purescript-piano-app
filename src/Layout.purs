module App.Layout where

import App.UI as UI
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)
import Pux.Html.Attributes
import Pux.Html.Attributes (style)
import Pux.Html.Elements
  
data Action
  = Child (UI.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: UI.State }

init :: State
init =
  { route: NotFound
  , count: UI.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (Child action) state = state { count = UI.update action state.count }

view :: State -> Html Action
view state =
  div
    [ id_ "main", style { width : "1260px"
                        , height : "700px" 
                        , overflow : "hidden" } ] 
    [ case state.route of
        Home -> map Child $ UI.view state.count
        NotFound -> App.NotFound.view state
    ]
    
