module Level exposing (..)

import Action exposing (..)
import Html exposing (..)
import Html.Attributes as Hats


type Level
    = Introduction
    | Easy
    | Medium
    | Hard
    | Infinite


levelAfter : Level -> Level
levelAfter l =
    case l of
        Introduction ->
            Easy

        Easy ->
            Medium

        Medium ->
            Hard

        Hard ->
            Infinite

        Infinite ->
            Infinite


tutorialForLevel : Level -> Html msg
tutorialForLevel l =
    case l of
        Introduction ->
            h3 [] [ text "Press LeftArrow to step left, RightArrow to step right" ]

        Easy ->
            h3 [] [ text "Press UpArrow to jump, DownArrow to clap." ]

        Medium ->
            h3 [] [ text "Press LeftArrow to step left, RightArrow to step right" ]

        Hard ->
            h3 [] [ text "Press LeftArrow to step left, RightArrow to step right" ]

        Infinite ->
            h3 [] [ text "You are entering eternal mode. The dance will continue until you fall." ]


narrationForLevel : Level -> Html msg
narrationForLevel l =
    case l of
        Introduction ->
            h1 [] [ text "In my many years observing various cultures, I haven't before seen one such as this. The culture, like many, had a form of dance that they engaged in ritualisticly. " ]

        Easy ->
            h1 [] [ text "From the outside it seems like a simple, but busy dance. The moves were big, bombast was encouraged, but there wasn't much novel about it, as is necessary for all but the most niche of memes. Still, children would train for years to take part in the ritual as adults." ]

        Medium ->
            h1 [] [ text "But the longer I watched, the faster it got. 1, 2, 3, 4 - I tried to keep count but eventually they got faster and faster, spinning around the circle like dervishes, cutting grooves in the earth beneath them." ]

        Hard ->
            h1 [] [ text "The dance served many many purposes. It encouraged prosocial behaviors like mating and business, but could also itself lead to conflict, especially on the rare occasion a dancer dared to vary. It almost certainly marked the beginning of violence. " ]

        Infinite ->
            h1 [] [ text "After several cycles around the sun,  I left the humans to their devices. They continued their dance, punishing those who forgot the steps or tried to make up their own. I, on the other hand, resolved ever more solidly to create my own dance, certain at that naive time I was much freer from my society than these simple creatures." ]


bpmFor : Level -> Float
bpmFor l =
    case l of
        Introduction ->
            60

        Easy ->
            80

        Medium ->
            100

        Hard ->
            120

        Infinite ->
            180


songFor : Level -> List Action
songFor l =
    case l of
        _ ->
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



{- Easy ->
   Medium ->
   Hard ->
   Infinite ->
-}


routineFor : Level -> Routine
routineFor level =
    { bpm = bpmFor level
    , song = songFor level
    }
