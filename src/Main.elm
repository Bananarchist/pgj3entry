module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Animator
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Hats
import Html.Events as Hevs
import Json.Decode as Decode
import Level exposing (..)
import List exposing ((::), concat, head, tail)
import Sprites exposing (..)
import Task
import Time
import Tuple exposing (first, second)



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


tolerance =
    500.0



--ms range around beat for success


introDelay =
    2



--beats


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscription_list =
            [ animator
                |> Animator.toSubscription Tick model.dmodel
            , Time.every 100 Tick
            , Browser.Events.onKeyDown (Decode.map KeyPress keyDecoder)
            , Browser.Events.onAnimationFrameDelta Frame
            , Time.every 1000 Beat
            ]
    in
    Sub.batch subscription_list



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
    = Dancer Int Action


type Robot
    = Robot Int Action


type alias Routine =
    { bpm : Float
    , song : List Action
    }


type Screen
    = GameScreen Phase
    | MenuScreen Menu


type Menu
    = Title
    | Options
    | Scores
    | Credits


type Phase
    = Narrative Level
    | Dancing DanceModel


type alias Challenge =
    { action : Maybe Action
    , attempt : Maybe Action
    , timing : ( Float, Float )
    , evaluation : Evaluation
    }


type alias DanceModel =
    { start : Float
    , lastKey : Maybe Key
    , lastTick : Float
    , lastBeatTime : Float
    , beat : Int
    , paused : Bool
    , challenge : Challenge
    , routine : Routine
    , dancer : Animator.Timeline Dancer
    , robot : Animator.Timeline Dancer
    }


type alias Model =
    { dmodel : DanceModel
    }



{- With this model we
   -- Check Action
   -- If Nothing move on
   -- Else check run animation
   -- Test against song...?
-}


init : Int -> ( Model, Cmd Msg )
init currentTime =
    let
        ctime =
            currentTime |> toFloat

        dmodel : DanceModel
        dmodel =
            { start = ctime
            , lastKey = Nothing
            , lastBeatTime = ctime
            , beat = introDelay * -1
            , lastTick = ctime
            , paused = False
            , challenge = Challenge Nothing Nothing ( ctime, ctime + (1000 * introDelay) ) Passed
            , routine = Routine 60.0 introSong
            , dancer = Animator.init (Dancer 0 Idle)
            , robot = Animator.init (Dancer 0 Idle)
            }
    in
    ( { dmodel = dmodel }
    , Cmd.none
    )


type Msg
    = Tick Time.Posix
    | KeyPress Key
    | Frame Float
    | Beat Time.Posix


actionForKey : Key -> Action
actionForKey k =
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


updateDanceModel : Msg -> DanceModel -> DanceModel
updateDanceModel msg model =
    case msg of
        Beat tp ->
            let
                t =
                    tp
                        |> Time.posixToMillis
                        |> toFloat

                newBeatTime =
                    if beat >= 0 then
                        let
                            tdelta =
                                (model.lastBeatTime + 1000) - t

                            divisor =
                                if tdelta >= 0 then
                                    2

                                else
                                    -2

                            avgdelta =
                                tdelta / divisor
                        in
                        model.lastBeatTime + 1000 + avgdelta

                    else
                        model.lastBeatTime

                beat =
                    model.beat + 1
            in
            { model | lastBeatTime = newBeatTime, beat = beat }

        Frame delta ->
            model
                |> updateSprites

        KeyPress k ->
            let
                oldChallenge =
                    model.challenge

                newChallenge =
                    if oldChallenge.action == Nothing || oldChallenge.evaluation == Failed then
                        oldChallenge

                    else
                        { oldChallenge | attempt = Just (actionForKey k) }
            in
            { model | lastKey = Just k, challenge = newChallenge }

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
                    model.routine.song

                newChallenge =
                    let
                        withinWindow =
                            tInMs >= first model.challenge.timing && tInMs <= second model.challenge.timing
                    in
                    if model.beat >= 0 then
                        case model.challenge.evaluation of
                            Passed ->
                                if model.challenge.action == Nothing || not withinWindow then
                                    nextChallenge model

                                else
                                    model.challenge

                            Indeterminate ->
                                let
                                    newEval =
                                        evaluationForIndeterminateChallenge model.challenge tInMs

                                    curChallenge =
                                        model.challenge
                                in
                                { curChallenge | evaluation = newEval }

                            Failed ->
                                model.challenge

                    else
                        model.challenge

                newModel =
                    { model
                        | lastTick = newLastTick
                        , start = newStartTime
                        , challenge = newChallenge
                    }
            in
            newModel
                |> Animator.update t animator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        dmodel =
            updateDanceModel msg model.dmodel
    in
    ( { model | dmodel = dmodel }
    , Cmd.none
    )


{-| case msg of
Beat tp ->
let
t =
tp
|> Time.posixToMillis
|> toFloat

                newBeatTime =
                    if beat >= 0 then
                        let
                            tdelta =
                                (model.lastBeatTime + 1000) - t

                            divisor =
                                if tdelta >= 0 then
                                    2

                                else
                                    -2

                            avgdelta =
                                tdelta / divisor
                        in
                        model.lastBeatTime + 1000 + avgdelta

                    else
                        model.lastBeatTime

                beat =
                    model.beat + 1
            in
            ( { model | lastBeatTime = newBeatTime, beat = beat }
            , Cmd.none
            )

        Frame delta ->
            ( model
                |> updateSprites
            , Cmd.none
            )

        KeyPress k ->
            let
                oldChallenge =
                    model.challenge

                newChallenge =
                    if oldChallenge.action == Nothing || oldChallenge.evaluation == Failed then
                        oldChallenge

                    else
                        { oldChallenge | attempt = Just (actionForKey k) }
            in
            ( { model | lastKey = Just k, challenge = newChallenge }
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
                    model.song

                newChallenge =
                    let
                        withinWindow =
                            tInMs >= first model.challenge.timing && tInMs <= second model.challenge.timing
                    in
                    if model.beat >= 0 then
                        case model.challenge.evaluation of
                            Passed ->
                                if model.challenge.action == Nothing || not withinWindow then
                                    nextChallenge model

                                else
                                    model.challenge

                            Indeterminate ->
                                let
                                    newEval =
                                        evaluationForIndeterminateChallenge model.challenge tInMs

                                    curChallenge =
                                        model.challenge
                                in
                                { curChallenge | evaluation = newEval }

                            Failed ->
                                model.challenge

                    else
                        model.challenge

                newModel =
                    { model
                        | song = newSong
                        , lastTick = newLastTick
                        , start = newStartTime
                        , challenge = newChallenge
                    }
            in
            ( newModel
                |> Animator.update t animator
            , Cmd.none
            )

-}
evaluationForIndeterminateChallenge challenge currentTime =
    let
        beforeTestWindow =
            currentTime < first challenge.timing

        afterTestWindow =
            currentTime > second challenge.timing

        withinTestWindow =
            not (afterTestWindow || beforeTestWindow)

        idleForAction =
            challenge.action == Just Idle
    in
    if challenge.attempt == challenge.action then
        if withinTestWindow then
            Passed

        else
            Failed

    else
        case challenge.attempt of
            Nothing ->
                if afterTestWindow then
                    if not idleForAction then
                        Failed

                    else
                        Passed

                else
                    Indeterminate

            Just _ ->
                Failed



-- just add the total pause time to start time on resume


getActionForBeat : List Action -> Float -> Float -> Int -> Maybe Action
getActionForBeat song bpm fromTime forBeat =
    let
        msPerBeat =
            bpm * 60 * 1000

        startPlusOneBeat =
            fromTime + msPerBeat

        andForOneLessBeat =
            forBeat - 1

        songTail =
            case tail song of
                Just s ->
                    s

                Nothing ->
                    [ Idle ]
    in
    if forBeat > 1 then
        getActionForBeat songTail bpm startPlusOneBeat andForOneLessBeat

    else
        head song


idleIfBeatSubZero beat action =
    if beat < 0 then
        Just Idle

    else
        action


nextChallenge : DanceModel -> Challenge
nextChallenge model =
    let
        beatTiming =
            model.lastBeatTime + 1000

        timing =
            ( beatTiming - tolerance, beatTiming + tolerance )

        challengeBeat =
            model.beat + 1

        nextAction =
            getActionForBeat model.routine.song 60 model.start challengeBeat
                |> idleIfBeatSubZero challengeBeat

        nextEval =
            case nextAction of
                Just _ ->
                    Indeterminate

                Nothing ->
                    Passed

        attempt =
            Nothing
    in
    { action = nextAction
    , attempt = attempt
    , timing = timing
    , evaluation = nextEval
    }


animator : Animator.Animator DanceModel
animator =
    Animator.animator
        |> Animator.watching .dancer (\dancer m -> { m | dancer = dancer })
        |> Animator.watching .robot (\robot m -> { m | robot = robot })


updateSprites : DanceModel -> DanceModel
updateSprites model =
    let
        current =
            Animator.current model.dancer

        offset =
            case current of
                Dancer o _ ->
                    o

        action =
            case model.lastKey of
                Just a ->
                    actionForKey a

                Nothing ->
                    Idle

        newDancer =
            Dancer offset action

        crobot =
            Animator.current model.robot

        roffset =
            case crobot of
                Dancer o _ ->
                    o

        animbeat =
            model.beat - 1

        raction =
            let
                actionForBeat =
                    getActionForBeat model.routine.song 60 model.start animbeat
                        |> idleIfBeatSubZero animbeat
            in
            case actionForBeat of
                Just a ->
                    a

                Nothing ->
                    Idle

        newRobot =
            Dancer roffset raction

        updateRobot =
            if model.start > 0.0 && crobot /= newRobot then
                model.robot |> Animator.go Animator.immediately newRobot

            else
                model.robot

        updateDancer =
            if model.start > 0.0 && current /= newDancer then
                model.dancer |> Animator.go Animator.immediately newDancer

            else
                model.dancer
    in
    { model | dancer = updateDancer, robot = updateRobot }


nextMoveIndicatorText : Challenge -> String
nextMoveIndicatorText challenge =
    case challenge.evaluation of
        Passed ->
            "NICE!"

        Indeterminate ->
            case challenge.action of
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



-- To get the rotation effect on the dancing circle
-- Draw set of dancers into div and warp the div whole


view : Model -> Html Msg
view model =
    let
        screen =
            viewDance model.dmodel
    in
    main_ [ Hats.id "gaem" ]
        [ screen ]


viewDance : DanceModel -> Html Msg
viewDance model =
    let
        next_move =
            if model.beat >= 0 then
                nextMoveIndicatorText model.challenge

            else
                String.fromInt model.beat
    in
    main_ [ Hats.id "gaem" ]
        [ section [ Hats.id "danceScreen" ]
            [ div [ Hats.id "dancers" ]
                (concat
                    [ List.repeat 3 (viewSprite (Animator.step model.robot <| spriteAnim))
                    , [ viewSprite (Animator.step model.dancer <| spriteAnim) ]
                    , List.repeat 3 (viewSprite (Animator.step model.robot <| spriteAnim))
                    ]
                )
            , div [ Hats.id "drummers" ]
                [ text next_move ]
            ]
        ]


spriteAnim (Dancer offset action) =
    let
        frame mySprite =
            Animator.frame mySprite

        directional dir mySprite =
            case dir of
                Left ->
                    translated 150 0 { mySprite | flipX = True }

                Right ->
                    mySprite

        translated xoffset yoffset mySprite =
            { mySprite | adjustX = xoffset, adjustY = yoffset }
    in
    case action of
        Idle ->
            frame sprite.tail.idle

        Clapping ->
            Animator.framesWith
                { transition = frame sprite.tail.idle
                , resting =
                    Animator.cycleN 1
                        (Animator.fps 5)
                        [ frame sprite.tail.clap1
                        , frame sprite.tail.clap2
                        , frame sprite.tail.idle
                        ]
                }

        Jumping ->
            Animator.framesWith
                { transition = frame sprite.tail.idle
                , resting =
                    Animator.cycleN 1
                        (Animator.fps 5)
                        (List.map frame
                            [ translated 0 20 sprite.tail.jump1
                            , sprite.tail.jump2
                            , translated 0 -20 sprite.tail.jump3
                            , sprite.tail.idle
                            ]
                        )
                }

        Stepping dir ->
            Animator.framesWith
                { transition = frame sprite.tail.idle
                , resting =
                    Animator.cycleN 1
                        (Animator.fps 5)
                        (List.map (frame << directional dir)
                            [ sprite.tail.step1
                            , sprite.tail.step2
                            , sprite.tail.idle
                            ]
                        )
                }


{-| dancer : Dancer -> Html Msg
dancer d =
case d of
Dancer offset action ->
div [ Hats.class "dancer", action |> actionClass |> Hats.class ]

        Crowd offset action ->
            div [ Hats.class "dancer", action |> actionClass |> Hats.class ] []

-}
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
