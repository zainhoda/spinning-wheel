import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Mouse
import Window

type Action = MousePosition (Int, Int)
            | MouseDown
            | MouseUp
            | NewTime Float
            | WindowSize (Int, Int)

type alias Model = { t: Float
                   , velocity: Float
                   , startMousePosition: (Int, Int)
                   , endMousePosition: (Int, Int)
                   , windowSize: (Int, Int)
                   , mouseIsDown: Bool
                    }

frictionFactor : Float
frictionFactor = 0.995

mousePushFactor : Float
mousePushFactor = 2

radius : Model -> Float
radius model =
  let x = fst model.windowSize
      y = snd model.windowSize
  in
    if x < y then (toFloat x)/2 else (toFloat y)/2

init : Model
init =
  Model 0 200 (0, 0) (0, 0) (400, 400) False

main : Signal Element
main =
  Signal.map view (Signal.foldp update init signal)

signal : Signal Action
signal =
  (Signal.mergeMany
    [Signal.map MousePosition Mouse.position
    ,Signal.map (\upDown -> if upDown then MouseDown else MouseUp) Mouse.isDown
    ,Signal.map NewTime (every millisecond)
    ,Signal.map WindowSize Window.dimensions
    ]
    )

update : Action -> Model -> Model
update action model =
  case action of
    MousePosition pos -> if model.mouseIsDown then {model | startMousePosition = pos} else {model | endMousePosition = pos}
    MouseDown -> {model | mouseIsDown = True}
    MouseUp -> {model | mouseIsDown = False, velocity = model.velocity + mousePushFactor * sqrt(toFloat ( ((fst model.startMousePosition)-(fst model.endMousePosition))^2 + ((snd model.startMousePosition) - (snd model.endMousePosition))^2 )) }
    NewTime t -> {model | t = model.t+model.velocity, velocity = if model.velocity > 0.05 then model.velocity * frictionFactor else 0}
    WindowSize size -> {model | windowSize = size}

view : Model -> Element
view model =
  collage (fst model.windowSize) (snd model.windowSize)
    [ filled lightGrey (circle (radius model))
    , outlined (solid grey) (circle (radius model))
    , hand orange (radius model - 10) model.t model.velocity
    ]


hand : Color -> Float -> Float -> Float -> Form
hand clr len time velocity =
  let
    angle = degrees (90 - 6 * inSeconds time)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)
