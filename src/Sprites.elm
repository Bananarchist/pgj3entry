module Sprites exposing (sprite, viewSprite)

import Html exposing (..)
import Html.Attributes as Hats


viewSprite : Box -> Html msg
viewSprite box =
    div
        [ Hats.style "position" "relative"
        , Hats.style "top" (String.fromInt box.adjustY ++ "px")
        , Hats.style "left" (String.fromInt box.adjustX ++ "px")
        , Hats.style "width" (String.fromInt box.width ++ "px")
        , Hats.style "height" (String.fromInt box.height ++ "px")
        , Hats.class "dancer"
        , Hats.style "transform"
            (if box.flipX then
                "scaleX(-1)"

             else
                "scaleX(1)"
            )
        , Hats.style "background-position"
            ("-"
                ++ (String.fromInt box.x ++ "px -")
                ++ (String.fromInt box.y ++ "px")
            )
        ]
        []


type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , adjustX : Int
    , adjustY : Int
    , flipX : Bool
    , flipY : Bool
    }


sprite =
    { tail =
        { idle =
            { x = 0
            , y = 1000
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , clap1 =
            { x = 0
            , y = 600
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , clap2 =
            { x = 150
            , y = 600
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , jump1 =
            { x = 0
            , y = 0
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , jump2 =
            { x = 150
            , y = 0
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , jump3 =
            { x = 300
            , y = 0
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step1 =
            { x = 0
            , y = 200
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step2 =
            { x = 150
            , y = 200
            , width = 150
            , height = 200
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        }
    }
