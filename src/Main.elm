module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Hats
import Html.Events as Hevs
import Json.Decode as Decode
import Level exposing (..)
import List exposing (head, tail)
import Task
import Time



-- constants


introSong =
    [ Stepping Left
    , Stepping Right
    , Jumping
    , Clapping
    , Stepping Right
    , Stepping Left
    , Clapping
    , Jumping
    , Clapping
    ]



-- how long before/after for a dance move to be right


tolerance =
    500.0



-- how long to wait before starting


introDelay =
    2500


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 Tick
        , Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)
        ]



-- Browser.Events.onAnimationFrame
--
-- on Frame event. take action and to make animation
-- then set to idle... right?


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


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


type Direction
    = Left
    | Right


type Action
    = Stepping Direction
    | Jumping
    | Clapping
    | Idle


type Evaluation
    = Passed
    | Failed
    | Indeterminate


type Key
    = Larrow
    | Rarrow
    | Darrow
    | Uarrow
    | Noarrow


type Dancer
    = Avatar Int Action
    | Crowd Int Action


type alias Challenge =
    { action : Maybe Action
    , timing : Float
    , evaluation : Evaluation
    }


type alias Model =
    { start : Float
    , action : Action
    , lastTick : Float
    , challenge : Challenge
    , song : List Action
    , dancer : Dancer
    }



{- With this model we
   -- Check Action
   -- If Nothing move on
   -- Else check run animation
   -- Test against song...?
-}


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( { start = toFloat currentTime
      , action = Idle
      , lastTick = toFloat currentTime
      , challenge = Challenge Nothing 0 Passed
      , song = introSong
      , dancer = Avatar 0 Idle
      }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | KeyPress Key
    | Frame Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame p ->
            let
                t =
                    p |> Time.posixToMillis |> toFloat
            in
            ( model
            , Cmd.none
            )

        KeyPress k ->
            let
                action =
                    case k of
                        Larrow ->
                            Stepping Left

                        Rarrow ->
                            Stepping Right

                        Uarrow ->
                            Jumping

                        Darrow ->
                            Clapping

                        Noarrow ->
                            Idle

                offset =
                    case model.dancer of
                        Avatar x a ->
                            x

                        Crowd x a ->
                            x
            in
            ( { model | action = action, dancer = Avatar offset action }
            , Cmd.none
            )

        Tick t ->
            let
                tInMs =
                    t
                        |> Time.posixToMillis
                        |> toFloat

                newStartTime =
                    if model.start == 0.0 then
                        tInMs

                    else
                        model.start

                newLastTick =
                    tInMs

                newSong =
                    case model.challenge.evaluation of
                        Failed ->
                            []

                        Indeterminate ->
                            model.song

                        Passed ->
                            let
                                tailOfSong =
                                    tail model.song
                            in
                            case tailOfSong of
                                Just songTail ->
                                    songTail

                                Nothing ->
                                    []

                newChallenge =
                    case model.challenge.evaluation of
                        Passed ->
                            nextChallenge model

                        Indeterminate ->
                            let
                                earliestTiming =
                                    model.challenge.timing - tolerance

                                latestTiming =
                                    model.challenge.timing + tolerance

                                newEval =
                                    case model.action of
                                        Idle ->
                                            if tInMs <= latestTiming then
                                                Indeterminate

                                            else
                                                Failed

                                        _ ->
                                            case model.challenge.action of
                                                Just a ->
                                                    if model.action == a then
                                                        if tInMs <= latestTiming && tInMs >= earliestTiming then
                                                            Passed

                                                        else
                                                            Failed

                                                    else
                                                        Failed

                                                Nothing ->
                                                    Indeterminate

                                curChallenge =
                                    model.challenge
                            in
                            { curChallenge | evaluation = newEval }

                        Failed ->
                            model.challenge

                newModel =
                    { model
                        | song = newSong
                        , action = Idle
                        , lastTick = newLastTick
                        , start = newStartTime
                        , challenge = newChallenge
                    }
            in
            ( newModel
            , Cmd.none
            )


nextChallenge : Model -> Challenge
nextChallenge model =
    let
        timing =
            model.lastTick + 1000

        queued =
            head model.song

        nextAction =
            queued

        nextEval =
            case queued of
                Just _ ->
                    Indeterminate

                Nothing ->
                    Passed
    in
    { action = nextAction
    , timing = timing
    , evaluation = nextEval
    }



-- To get the rotation effect on the dancing circle
-- Draw set of dancers into div and warp the div whole


view : Model -> Html Msg
view model =
    let
        next_move =
            case model.challenge.evaluation of
                Passed ->
                    "NICE!"

                Indeterminate ->
                    case model.challenge.action of
                        Just s ->
                            case s of
                                Stepping d ->
                                    case d of
                                        Left ->
                                            "Step left!"

                                        Right ->
                                            "Step right!"

                                Jumping ->
                                    "Jump!"

                                Clapping ->
                                    "Clap!"

                                Idle ->
                                    "Wait for it..."

                        Nothing ->
                            "Wait for it..."

                Failed ->
                    "Miss! :("
    in
    main_ [ Hats.id "gaem" ]
        [ section [ Hats.id "danceScreen" ]
            [ div [ Hats.id "dancers" ]
                [ dancer model.dancer ]
            , div [ Hats.id "drummers" ]
                [ text next_move ]
            ]
        ]


dancer : Dancer -> Html Msg
dancer d =
    case d of
        Avatar offset action ->
            div [ Hats.class "dancer", action |> actionClass |> Hats.class ] []

        Crowd offset action ->
            div [ Hats.class "dancer", action |> actionClass |> Hats.class ] []


actionClass : Action -> String
actionClass action =
    let
        anim =
            case action of
                Stepping d ->
                    case d of
                        Left ->
                            "leftstep"

                        Right ->
                            "rightstep"

                Jumping ->
                    "jump"

                Clapping ->
                    "clap"

                Idle ->
                    "none"
    in
    "sprite_" ++ anim
