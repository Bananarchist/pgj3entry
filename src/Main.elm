module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Action exposing (..)
import Animator
import Browser
import Browser.Events
import Config exposing (..)
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


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


keyDecoder : Config -> Decode.Decoder Key
keyDecoder config =
    Decode.map (toKey config) (Decode.field "key" Decode.string)


keyPassThrough : Decode.Decoder String
keyPassThrough =
    Decode.map (\s -> s) (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscription_list =
            [ animator
                |> Animator.toSubscription Tick model.dmodel
            , Time.every 100 Tick
            , Browser.Events.onKeyDown (Decode.map KeyPress (Decode.field "key" Decode.string))

            --, Browser.Events.onKeyDown (Decode.map KeyPress (keyDecoder model.config))
            , Browser.Events.onAnimationFrameDelta Frame
            , Time.every (60000 / model.dmodel.routine.bpm * model.config.bpm) Beat
            ]
    in
    Sub.batch subscription_list


type Evaluation
    = Passed
    | Failed
    | Indeterminate


type Dancer
    = Dancer Int Action


type Screen
    = GameScreen Phase
    | MenuScreen Menu


type Menu
    = Title
    | Options (Maybe Key)
    | Scores
    | Credits


type Phase
    = Narrative
    | Dancing


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
    , score : Int
    }


danceModelFor : Routine -> DanceModel
danceModelFor routine =
    { start = 0
    , lastKey = Nothing
    , lastTick = 0
    , lastBeatTime = 0
    , beat = 0
    , paused = False
    , challenge = Challenge Nothing Nothing ( 0, 0 ) Indeterminate
    , routine = routine
    , dancer = Animator.init (Dancer 0 Idle)
    , robot = Animator.init (Dancer 0 Idle)
    , score = 0
    }


danceModelLastTickFrom : Float -> DanceModel -> DanceModel
danceModelLastTickFrom tick dmodel =
    { dmodel | lastTick = tick }


danceModelStartFrom : Float -> DanceModel -> DanceModel
danceModelStartFrom tick dmodel =
    { dmodel | start = tick }


type alias Model =
    { screen : Screen
    , level : Level
    , dmodel : DanceModel
    , config : Config
    }


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
            , beat = 0
            , lastTick = ctime
            , paused = False
            , challenge = Challenge Nothing Nothing ( ctime, ctime + 1000 ) Passed
            , routine = routineFor Introduction
            , dancer = Animator.init (Dancer 0 Idle)
            , robot = Animator.init (Dancer 0 Idle)
            , score = 0
            }
    in
    ( { screen = MenuScreen Title
      , level = Introduction
      , dmodel = dmodel
      , config = defaultConfig
      }
    , Cmd.none
    )


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


justLastTick msg dmodel =
    case msg of
        Tick tick ->
            { dmodel | lastTick = (Time.posixToMillis >> toFloat) tick }

        _ ->
            dmodel


withDanceModelForMsg : Msg -> Model -> Model
withDanceModelForMsg msg parentmodel =
    let
        model =
            parentmodel.dmodel

        dmodel =
            case parentmodel.screen of
                MenuScreen _ ->
                    justLastTick msg model

                GameScreen phase ->
                    case phase of
                        Narrative ->
                            justLastTick msg model

                        Dancing ->
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

                                KeyPress s ->
                                    let
                                        k =
                                            s |> toKey parentmodel.config

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

                                        withinWindow =
                                            tInMs >= first model.challenge.timing && tInMs <= second model.challenge.timing

                                        newChallenge =
                                            if model.beat >= 0 then
                                                case model.challenge.evaluation of
                                                    Passed ->
                                                        if model.challenge.action == Nothing || not withinWindow then
                                                            nextChallenge parentmodel

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

                                        newScore =
                                            if model.challenge /= newChallenge && model.beat > 0 then
                                                model.score + 1

                                            else
                                                model.score

                                        newModel =
                                            { model
                                                | lastTick = newLastTick
                                                , score = newScore
                                                , start = newStartTime
                                                , challenge = newChallenge
                                            }
                                    in
                                    newModel
                                        |> Animator.update t animator

                                _ ->
                                    model
    in
    if model.beat > List.length model.routine.song && parentmodel.level /= Infinite then
        case parentmodel.screen of
            GameScreen Dancing ->
                { parentmodel
                    | dmodel = dmodel
                    , screen = GameScreen Narrative
                    , level = levelAfter parentmodel.level
                }

            _ ->
                { parentmodel | dmodel = dmodel }

    else
        { parentmodel | dmodel = dmodel }


type Msg
    = Tick Time.Posix
    | KeyPress String
    | Frame Float
    | Beat Time.Posix
    | ScreenTransition Screen
    | PhaseShift Phase
    | StartGame DanceModel
    | StartStory Level
    | SettingsScreenInput ConfigSettingTextEntry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SettingsScreenInput confentry ->
            case confentry of
                BPMText s ->
                    let
                        bpm =
                            s
                                |> String.toFloat
                                |> Maybe.withDefault 0.0
                                |> clamp 0.5 3.0
                    in
                    ( model |> withBpm bpm
                    , Cmd.none
                    )

                ToleranceText s ->
                    let
                        tolerance =
                            s
                                |> String.toFloat
                                |> Maybe.withDefault 0.5
                                |> clamp 0.0 1.0
                    in
                    ( model |> withTolerance tolerance
                    , Cmd.none
                    )

                CountInText s ->
                    let
                        countin =
                            s
                                |> String.toInt
                                |> Maybe.withDefault 0
                                |> clamp 0 8
                    in
                    ( model |> withCountIn countin
                    , Cmd.none
                    )

        KeyPress s ->
            case model.screen of
                GameScreen _ ->
                    ( model |> withDanceModelForMsg msg
                    , Cmd.none
                    )

                MenuScreen menu ->
                    case menu of
                        Options opt ->
                            case opt of
                                Just a ->
                                    let
                                        newmodel =
                                            { model | screen = MenuScreen (Options Nothing) }
                                    in
                                    ( newmodel |> withArrow a s
                                    , Cmd.none
                                    )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        ScreenTransition screen ->
            ( { model | screen = screen }
            , Cmd.none
            )

        StartGame dmodel ->
            ( { model | screen = GameScreen Dancing, dmodel = dmodel }
            , Cmd.none
            )

        StartStory level ->
            ( { model | screen = GameScreen Narrative, level = level }
            , Cmd.none
            )

        PhaseShift phase ->
            case phase of
                Narrative ->
                    ( { model | screen = GameScreen Narrative, level = levelAfter model.level }
                    , Cmd.none
                    )

                Dancing ->
                    let
                        dmodel =
                            model.dmodel

                        newdmodel =
                            danceModelFor (routineFor model.level)
                                |> danceModelLastTickFrom dmodel.lastTick
                                |> danceModelStartFrom dmodel.lastTick
                                |> (\d -> { d | beat = 1 - model.config.countin })
                                |> (\d -> { d | lastBeatTime = dmodel.lastTick })
                                |> (\d -> { d | challenge = Challenge Nothing Nothing ( 0, 0 ) Passed })
                    in
                    ( { model
                        | dmodel = newdmodel
                        , screen = GameScreen Dancing
                      }
                    , Cmd.none
                    )

        _ ->
            ( model |> withDanceModelForMsg msg
            , Cmd.none
            )


evaluationForIndeterminateChallenge : Challenge -> Float -> Evaluation
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


nextChallenge : Model -> Challenge
nextChallenge { dmodel, config } =
    let
        model =
            dmodel

        beatTiming : Float
        beatTiming =
            if model.beat < 0 then
                toFloat model.beat * -1000 + model.lastBeatTime + 1000

            else
                model.lastBeatTime + 1000

        tolerance =
            config.tolerance / 2 * 60000 / (dmodel.routine.bpm * config.bpm)

        timing =
            ( beatTiming - tolerance, beatTiming + tolerance )

        challengeBeat =
            model.beat + 1

        nextAction =
            getActionForBeat model.routine.song 60 model.start challengeBeat

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


view : Model -> Html Msg
view model =
    let
        screen =
            case model.screen of
                MenuScreen menu ->
                    case menu of
                        Title ->
                            viewTitle model

                        Options jk ->
                            viewSettings model

                        Scores ->
                            viewScores

                        Credits ->
                            viewCredits

                GameScreen phase ->
                    case phase of
                        Dancing ->
                            viewDance model.dmodel

                        Narrative ->
                            viewNarrative model
    in
    main_ [ Hats.id "gaem" ]
        [ screen ]


viewTitle : Model -> Html Msg
viewTitle model =
    section [ Hats.id "titleScreen" ]
        [ h1 [] [ text "Pouliuli" ]
        , titleMenu model
        ]


titleMenu : Model -> Html Msg
titleMenu model =
    let
        continuedmodel =
            danceModelFor (routineFor model.level)
    in
    ul []
        [ li [] [ button [ Hevs.onClick (StartStory Introduction) ] [ text "begin" ] ]
        , li [] [ button [ Hevs.onClick (StartGame continuedmodel) ] [ text "continue" ] ]
        , li [] [ button [ Hevs.onClick (ScreenTransition (MenuScreen (Options Nothing))) ] [ text "options" ] ]

        --, li [] [ button [ Hevs.onClick (ScreenTransition (MenuScreen Scores)) ] [ text "review attempts" ] ]
        , li [] [ button [ Hevs.onClick (ScreenTransition (MenuScreen Credits)) ] [ text "credits" ] ]
        ]


backToTitleBtn =
    button [ Hats.class "backToTitle", Hevs.onClick (ScreenTransition (MenuScreen Title)) ] [ text (String.fromChar (Char.fromCode 8592)) ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        recordKey : Key -> Msg
        recordKey =
            Just >> Options >> MenuScreen >> ScreenTransition
    in
    section [ Hats.id "settingsScreen" ]
        [ backToTitleBtn
        , h1 [] [ text "Update settings" ]
        , div []
            (concat
                [ settingsLabel (button [ Hevs.onClick (recordKey Larrow) ] [ text ("Record over " ++ stringForArrowMapping model.config.larrow) ]) "Left Step Key"
                , settingsLabel (button [ Hevs.onClick (recordKey Rarrow) ] [ text ("Record over " ++ stringForArrowMapping model.config.rarrow) ]) "Right Step Key"
                , settingsLabel (button [ Hevs.onClick (recordKey Uarrow) ] [ text ("Record over " ++ stringForArrowMapping model.config.uarrow) ]) "Jump Key"
                , settingsLabel (button [ Hevs.onClick (recordKey Darrow) ] [ text ("Record over " ++ stringForArrowMapping model.config.darrow) ]) "Clap Key"
                , settingsLabel (input [ Hats.value (String.fromFloat model.config.bpm), Hevs.onInput (SettingsScreenInput << BPMText) ] []) "BPM Multiplier"
                , settingsLabel (input [ Hats.value (String.fromFloat model.config.tolerance), Hevs.onInput (SettingsScreenInput << ToleranceText) ] []) "Accuracy tolerance"
                , settingsLabel (input [ Hats.value (String.fromInt model.config.countin), Hevs.onInput (SettingsScreenInput << CountInText) ] []) "Count-in"
                ]
            )
        ]


settingsLabel : Html Msg -> String -> List (Html Msg)
settingsLabel input string =
    [ label [] [ text string ]
    , input
    ]


viewScores : Html Msg
viewScores =
    section [ Hats.id "scoresScreen" ] [ backToTitleBtn, text "Nobody has scored anything yet" ]


viewCredits : Html Msg
viewCredits =
    section [ Hats.id "creditsScreen" ] [ backToTitleBtn, h1 [] [ text "ALL CREDIT TO THE BANANARCHIST" ] ]


viewNarrative : Model -> Html Msg
viewNarrative model =
    let
        narrativeText =
            narrationForLevel model.level

        tutorialText =
            tutorialForLevel model.level
    in
    section [ Hats.id "narrationScreen" ]
        [ narrativeText
        , tutorialText
        , button [ Hevs.onClick (PhaseShift Dancing) ] [ text "continue" ]
        ]


viewDance : DanceModel -> Html Msg
viewDance model =
    let
        next_move =
            if model.beat >= 0 then
                nextMoveIndicatorText model.challenge

            else
                String.fromInt model.beat

        score =
            String.fromInt model.beat
    in
    main_ [ Hats.id "gaem" ]
        [ section [ Hats.id "danceScreen" ]
            [ div [ Hats.id "score" ] [ text score ]
            , div [ Hats.id "dancers" ]
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
