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


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


defaultConfig : Config
defaultConfig =
    { larrow = ArrowMap Larrow "ArrowLeft"
    , rarrow = ArrowMap Larrow "ArrowLeft"
    , darrow = ArrowMap Larrow "ArrowLeft"
    , uarrow = ArrowMap Larrow "ArrowLeft"
    , bpm = 1.0
    , tolerance = 0.5
    , countin = 2
    }


toKey : String -> Key
toKey string =
    case string of
        "ArrowLeft" ->
            Larrow

        "ArrowRight" ->
            Rarrow

        "ArrowDown" ->
            Darrow

        "ArrowUp" ->
            Uarrow

        _ ->
            Noarrow


type Key
    = Larrow
    | Rarrow
    | Darrow
    | Uarrow
    | Noarrow
