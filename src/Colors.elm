module Colors exposing
    ( black
    , blue
    , blueGreen
    , grass
    , gray
    , green
    , lightBlue
    , orange
    , pink
    , purple
    , red
    , sky
    , white
    , yellow
    )

import Element exposing (Color)


red : Color
red =
    Element.rgb255 255 0 0


orange : Color
orange =
    Element.rgb255 255 153 51


yellow : Color
yellow =
    Element.rgb255 255 255 0


grass : Color
grass =
    Element.rgb255 127 255 0


green : Color
green =
    Element.rgb255 0 255 0


blueGreen : Color
blueGreen =
    Element.rgb255 0 255 128


lightBlue : Color
lightBlue =
    Element.rgb255 0 255 255


sky : Color
sky =
    Element.rgb255 0 127 255


blue : Color
blue =
    Element.rgb255 0 0 255


purple : Color
purple =
    Element.rgb255 127 0 255


pink : Color
pink =
    Element.rgb255 255 107 178


white : Color
white =
    Element.rgb255 255 255 255


black : Color
black =
    Element.rgb255 0 0 0


gray : Color
gray =
    Element.rgb255 128 128 128
