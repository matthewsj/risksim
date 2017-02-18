module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Ordering
import Random.Pcg as Random exposing (Generator)


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = (\msg model -> update msg model ! [])
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { maxAttackers : Int
    , maxDefenders : Int
    , numTrials : Int
    , winPercentagesTable : Maybe (Table Float)
    , seed : Random.Seed
    }


init : Model
init =
    { maxAttackers = 5
    , maxDefenders = 5
    , numTrials = 100
    , winPercentagesTable = Nothing
    , seed = Random.initialSeed 324242423
    }


type Msg
    = SetMaxAttackers Int
    | SetMaxDefenders Int
    | SetNumTrials Int
    | Recalculate
    | NoOp


update : Msg -> Model -> Model
update msg ({ maxAttackers, maxDefenders, numTrials, winPercentagesTable, seed } as model) =
    case msg of
        SetMaxAttackers newMax ->
            { model
                | maxAttackers = newMax
                , winPercentagesTable =
                    if maxAttackers == newMax then
                        winPercentagesTable
                    else
                        Nothing
            }

        SetMaxDefenders newMax ->
            { model
                | maxDefenders = newMax
                , winPercentagesTable =
                    if maxDefenders == newMax then
                        winPercentagesTable
                    else
                        Nothing
            }

        SetNumTrials newNumTrials ->
            { model
                | numTrials = newNumTrials
                , winPercentagesTable =
                    if numTrials == newNumTrials then
                        winPercentagesTable
                    else
                        Nothing
            }

        Recalculate ->
            let
                ( newTable, newSeed ) =
                    Random.step (generateTable maxAttackers maxDefenders numTrials) seed
            in
                { model
                    | winPercentagesTable = Just newTable
                    , seed = newSeed
                }

        NoOp ->
            model


flatten : List (Generator a) -> Generator (List a)
flatten generators =
    case generators of
        [] ->
            Random.constant []

        g :: gs ->
            g
                |> Random.andThen
                    (\gValue ->
                        flatten gs
                            |> Random.map (\gsValue -> gValue :: gsValue)
                    )


doubleFlatten : List (List (Generator a)) -> Generator (List (List a))
doubleFlatten elts =
    flatten (List.map flatten elts)


forEachN : Int -> Int -> (Int -> a) -> List a
forEachN lo hi f =
    List.map f (List.range lo hi)


generateTable : Int -> Int -> Int -> Generator (Table Float)
generateTable maxAttackers maxDefenders numTrials =
    forEachN 1
        maxAttackers
        (\numAttackers ->
            forEachN 1
                maxDefenders
                (\numDefenders ->
                    winPercent numAttackers numDefenders numTrials
                )
        )
        |> doubleFlatten


winPercent : Int -> Int -> Int -> Generator Float
winPercent attackers defenders trials =
    Random.list trials (battle attackers defenders)
        |> Random.map calculateAttackerWinPercent


calculateAttackerWinPercent : List BattleOutcome -> Float
calculateAttackerWinPercent outcomes =
    (List.length (List.filter (\x -> x == AttackerWins) outcomes) |> toFloat)
        / (List.length outcomes |> toFloat)


rolls : Int -> Generator (List Int)
rolls n =
    Random.list n (Random.int 1 6)


type alias SkirmishRecord =
    { attackerWins : Int
    , defenderWins : Int
    }


type BattleOutcome
    = AttackerWins
    | DefenderWins


wins : List Int -> List Int -> SkirmishRecord
wins attackerRoll defenderRoll =
    let
        sort =
            List.sortWith (Ordering.reverse Ordering.natural)

        sortedAttackerRolls =
            sort attackerRoll

        sortedDefenderRolls =
            sort defenderRoll

        loop : List Int -> List Int -> Int -> Int -> SkirmishRecord
        loop aRolls dRolls aWins dWins =
            case ( aRolls, dRolls ) of
                ( a :: aRolls, d :: dRolls ) ->
                    if a > d then
                        loop aRolls dRolls (aWins + 1) dWins
                    else
                        loop aRolls dRolls aWins (dWins + 1)

                ( _, _ ) ->
                    { attackerWins = aWins, defenderWins = dWins }
    in
        loop sortedAttackerRolls sortedDefenderRolls 0 0


skirmish : Int -> Int -> Generator SkirmishRecord
skirmish attackers defenders =
    Random.map2 wins (rolls attackers) (rolls defenders)


battle : Int -> Int -> Generator BattleOutcome
battle attackers defenders =
    if attackers == 0 then
        Random.constant DefenderWins
    else if defenders == 0 then
        Random.constant AttackerWins
    else
        skirmish attackers defenders
            |> Random.andThen
                (\{ attackerWins, defenderWins } ->
                    battle (attackers - defenderWins) (defenders - attackerWins)
                )


parseNumber : (Int -> Msg) -> String -> Msg
parseNumber action str =
    case String.toInt str of
        Ok n ->
            if n > 0 then
                action n
            else
                NoOp

        Err msg ->
            NoOp


view : Model -> Html Msg
view { maxAttackers, maxDefenders, numTrials, winPercentagesTable } =
    div []
        [ text "Max attackers: "
        , input
            [ type_ "number"
            , value (toString maxAttackers)
            , onInput (parseNumber SetMaxAttackers)
            ]
            []
        , br [] []
        , text "Max defenders: "
        , input
            [ type_ "number"
            , value (toString maxDefenders)
            , onInput (parseNumber SetMaxDefenders)
            ]
            []
        , br [] []
        , text "Number of trials: "
        , input
            [ type_ "number"
            , value (toString numTrials)
            , onInput (parseNumber SetNumTrials)
            ]
            []
        , br [] []
        , button [ onClick Recalculate ] [ text "Recalculate" ]
        , displayWinPercentagesTable winPercentagesTable
        ]


displayWinPercentagesTable : Maybe (Table Float) -> Html a
displayWinPercentagesTable maybeTable =
    case maybeTable of
        Nothing ->
            div [] [ text "Click \"Recalculate\" to see results" ]

        Just percentagesTable ->
            let
                row colValues =
                    tr [] (List.map (\f -> td [style (forPercent f)] [ toString f |> text ]) colValues)
            in
                table []
                    (List.map row percentagesTable)

forPercent : Float -> List (String, String)
forPercent percent =
    let (color, intensity) =
            if percent < 0.5 then
                (0, (0.5 - percent) * 200 |> round)
            else
                (120, (percent - 0.5) * 200 |> round)
    in
        Debug.log "value"
        [ ("background-color", "hsl(" ++ (toString color) ++ ", " ++ (toString intensity) ++ "%, 50%)")
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Table a =
    List (List a)
