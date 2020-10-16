module Config exposing (..)

import Json.Decode as Decode


type ArrowMap
    = ArrowMap Key String


type alias Config =
    { larrow : ArrowMap
    , rarrow : ArrowMap
    , darrow : ArrowMap
    , uarrow : ArrowMap
    , bpm : Float
    , tolerance : Float
    , countin : Int
    }


defaultConfig : Config
defaultConfig =
    { larrow = ArrowMap Larrow "ArrowLeft"
    , rarrow = ArrowMap Rarrow "ArrowRight"
    , darrow = ArrowMap Darrow "ArrowDown"
    , uarrow = ArrowMap Uarrow "ArrowUp"
    , bpm = 1.0
    , tolerance = 0.5
    , countin = 2
    }


stringForArrowMapping : ArrowMap -> String
stringForArrowMapping m =
    case m of
        ArrowMap _ s ->
            s


toKey : Config -> String -> Key
toKey config string =
    let
        arrowLeft =
            stringForArrowMapping config.larrow
                |> (==) string

        arrowRight =
            stringForArrowMapping config.rarrow
                |> (==) string

        arrowDown =
            stringForArrowMapping config.darrow
                |> (==) string

        arrowUp =
            stringForArrowMapping config.uarrow
                |> (==) string
    in
    if arrowLeft then
        Larrow

    else if arrowRight then
        Rarrow

    else if arrowDown then
        Darrow

    else if arrowUp then
        Uarrow

    else
        Noarrow


type Key
    = Larrow
    | Rarrow
    | Darrow
    | Uarrow
    | Noarrow
