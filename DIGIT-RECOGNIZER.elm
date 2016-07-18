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
  , drawing : Bool
  , output : Maybe Int}


type Coords
  = Coords Int Int

init : ( Model, Cmd Msg ) 
init = 
  ( Model [] False Nothing, Cmd.none)

-- UPDATE

type Msg
  = DrawingStart Position
  | DrawingAt Position
  | DrawingEnd Position
  | Reset
  | Recognize

update  : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( update' msg model, Cmd.none )

update' : Msg -> Model -> Model
update' msg model = 
  case msg of
    Reset -> 
      Model [] False Nothing
      
    Recognize ->
      Model model.canvas model.drawing (Just (recognize model.canvas))
      
    DrawingStart xy ->
      Model (insertPoint (convert xy) model.canvas) True Nothing

    DrawingAt xy ->
      Model (insertPoint (convert xy) model.canvas) True Nothing

    DrawingEnd xy ->
      Model (insertPoint (convert xy) model.canvas) False Nothing

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
        , y (toString (b * 10))
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
             , Html.button [ onClick Recognize ] [ Html.text "Recognize" ]
             ,Html.h2 [] [ Html.text (toString model.output)]
             ]

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DrawingStart Mouse.position)

--Recognize

recognize : List Coords -> Int
recognize canvas = 
  assign (round ((toFloat (10*(List.length canvas)))/(28*28)))


assign : Int -> Int
assign n = case n of 
  0 -> 1
  1 -> 2 
  2 -> 7
  3 -> 4
  4 -> 0
  5 -> 3
  6 -> 5
  7 -> 6
  8 -> 9
  _ -> 8

  

















