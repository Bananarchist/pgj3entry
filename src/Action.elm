module Action exposing (..)


type Action
    = Stepping Direction
    | Jumping
    | Clapping
    | Idle


type Direction
    = Left
    | Right


type alias Routine =
    { bpm : Float
    , song : List Action
    }
