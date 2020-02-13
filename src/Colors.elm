module Colors exposing
    ( backgroundBlue
    , backgroundRed
    , black
    , blue
    , blueGreen
    , brown
    , grass
    , gray
    , green
    , lightBlue
    , naturalColor
    , orange
    , pink
    , purple
    , red
    , sky
    , veryLightBlue
    , white
    , yellow
    )

import Element exposing (Color)


red : Color
red =
    Element.rgb255 200 0 0


orange : Color
orange =
    Element.rgb255 255 153 51


yellow : Color
yellow =
    Element.rgb255 200 200 0


grass : Color
grass =
    Element.rgb255 127 255 0


green : Color
green =
    Element.rgb255 0 200 0


blueGreen : Color
blueGreen =
    Element.rgb255 0 255 128


lightBlue : Color
lightBlue =
    Element.rgb255 0 0 200


sky : Color
sky =
    Element.rgb255 0 180 255


blue : Color
blue =
    Element.rgb255 0 0 200


purple : Color
purple =
    Element.rgb255 127 0 255


pink : Color
pink =
    Element.rgb255 221 136 221


white : Color
white =
    Element.rgb255 255 255 255


black : Color
black =
    Element.rgb255 0 0 0


gray : Color
gray =
    Element.rgb255 102 102 102


veryLightBlue : Color
veryLightBlue =
    Element.rgb255 153 204 255


backgroundBlue : Color
backgroundBlue =
    Element.rgb255 68 122 186


backgroundRed : Color
backgroundRed =
    Element.rgb255 255 77 77


naturalColor : Color
naturalColor =
    Element.rgb255 178 102 255


brown : Color
brown =
    Element.rgb255 179 111 74
