import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Mouse

type Action = MousePosition (Int, Int)
            | MouseDown
            | MouseUp
            | NewTime Float

type alias Model = { t: Float
                   , velocity: Float
                   , lastMousePosition: (Int, Int)
                   , mouseIsDown: Bool
                    }

init : Model
init =
  Model 0 20 (0, 0) False

main : Signal Element
main =
  Signal.map view (Signal.foldp update init signal)

signal : Signal Action
signal =
  (Signal.mergeMany
    [Signal.map MousePosition Mouse.position
    ,Signal.map (\upDown -> if upDown then MouseDown else MouseUp) Mouse.isDown
    ,Signal.map NewTime (every millisecond)
    ]
    )

update : Action -> Model -> Model
update action model =
  case action of
    MousePosition pos -> {model | lastMousePosition = pos}
    MouseDown -> {model | mouseIsDown = True, velocity = model.velocity * 2}
    MouseUp -> {model | mouseIsDown = False}
    NewTime t -> {model | t = t}

view : Model -> Element
view model =
  collage 400 400
    [ filled lightGrey (circle 110)
    , outlined (solid grey) (circle 110)
    , hand orange 100 model.t model.velocity
    ]


hand : Color -> Float -> Float -> Float -> Form
hand clr len time velocity =
  let
    angle = degrees (90 - 6 * inSeconds time * velocity)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)
