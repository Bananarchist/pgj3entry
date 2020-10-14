module Level exposing (..)


type Level
    = Introduction
    | Easy
    | Medium
    | Hard
    | Infinite


narrationForLevel : Level -> ( String, String )
narrationForLevel l =
    case l of
        Introduction ->
            ( "I had never really encountered anything like it. They were all completely in sync. They danced beat by beat on their highly trained feet. None would dare step out of turn."
            , "Press LeftArrow to step left, RightArrow to step right"
            )

        Easy ->
            ( "It was amazing to watch. Children would train for years to take part in the ritual as adults."
            , "Press UpArrow to jump, DownArrow to clap."
            )

        Medium ->
            ( "1, 2, 3, 4 - I tried to keep count but eventually they got faster and faster, spinning around the circle like dervishes, cutting grooves in the earth beneath them."
            , ""
            )

        Hard ->
            ( "The longer I watched the more I wondered why they never varied. There was never a single one who even attempted an idiosyncratic embellishment of the moves, none who risked being different."
            , ""
            )

        Infinite ->
            ( "Eventually I left the humans to their devices. They continued their dance, punishing those who forgot the steps or tried to make up their own. I, on the other hand, resolved ever more solidly to create my own dance, to live life by my own rhythm."
            , "You are entering eternal mode - the dance will continue until you fail"
            )
