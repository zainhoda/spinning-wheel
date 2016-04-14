import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)


main : Signal Element
main =
  Signal.map clock (every millisecond)


clock : Float -> Element
clock t =
  collage 400 400
    [ filled lightGrey (ngon 12 110)
    , outlined (solid grey) (ngon 12 110)
    , hand orange 100 t 20
    ]


hand : Color -> Float -> Float -> Float -> Form
hand clr len time velocity =
  let
    angle = degrees (90 - 6 * inSeconds time * velocity)
  in
    segment (0,0) (fromPolar (len,angle))
      |> traced (solid clr)
