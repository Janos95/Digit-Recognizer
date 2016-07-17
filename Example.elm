module Main exposing (..)

import Html 
import Html.Events exposing (on,onClick)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)

main =
  App.program
    {init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions 
    }

-- Model

type alias Model = 
  {canvas : List Coords
  , drawing : Bool}


type Coords
  = Coords Int Int

init : ( Model, Cmd Msg ) 
init = 
  ( Model [] False , Cmd.none)

-- UPDATE

type Msg
  = DrawingStart Position
  | DrawingAt Position
  | DrawingEnd Position
  | Reset

update  : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( update' msg model, Cmd.none )

update' : Msg -> Model -> Model
update' msg model = 
  case msg of
    Reset -> 
      Model [] False

    DrawingStart xy ->
      Model (insertPoint (convert xy) model.canvas) True

    DrawingAt xy ->
      Model (insertPoint (convert xy) model.canvas) True

    DrawingEnd xy ->
      Model (insertPoint (convert xy) model.canvas) False

convert : Position -> Coords
convert position = Coords (position.x // 10) (position.y // 10)

insertPoint : Coords -> List Coords -> List Coords
insertPoint point points = case List.member point points of 
  True -> 
    points
  False ->
    point :: points

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drawing of
    False -> 
      Sub.none

    True -> 
      Sub.batch [ Mouse.moves DrawingAt, Mouse.ups DrawingEnd ]



-- VIEW


plotSquares : List Coords -> Html.Html msg
plotSquares =
  let
    toSquare (Coords a b) =
      rect
        [ x (toString (a * 10))
        , y (toString (280 - (b * 10)))
        , width "10"
        , height "10" ] []
  in
    svg [ width "280", height "280" ] << List.map toSquare


view : Model -> Html.Html Msg
view model =
  let 
    points = 
      model.canvas
  in 
    Html.div []
             [ Html.div [ onMouseDown ]
               [ plotSquares <| points ]
             , Html.button [onClick Reset] [ Html.text "Reset"]
             ]

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DrawingStart Mouse.position)








