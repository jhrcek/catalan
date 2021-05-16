module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode exposing (Decoder)
import Random
import Random.List
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), StrokeLinecap(..), Transform(..), px)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { n : Int
    , path : Path
    }


type Step
    = Down
    | Up


type alias Path =
    List Step


type Msg
    = AddUp
    | AddDown
    | AddRandomUpOrDown
    | RemoveLastStep
    | SetN String
    | GenerateRandomLatticePath
    | GenerateRandomCatalanPath
    | SetPath Path
    | SetSmallestPath
    | SetNextPath


init () =
    ( { n = 5
      , path = [ Up, Up, Down, Up, Up, Up, Down, Down, Down, Down ]
      }
    , Cmd.none
    )


update msg model =
    case msg of
        RemoveLastStep ->
            ( { model | path = List.take (List.length model.path - 1) model.path }
            , Cmd.none
            )

        AddUp ->
            let
                ( ups, _ ) =
                    countUpsDowns model.path
            in
            if ups >= model.n then
                ( model, Cmd.none )

            else
                ( { model | path = model.path ++ [ Up ] }, Cmd.none )

        AddDown ->
            let
                ( _, downs ) =
                    countUpsDowns model.path
            in
            if downs >= model.n then
                ( model, Cmd.none )

            else
                ( { model | path = model.path ++ [ Down ] }, Cmd.none )

        AddRandomUpOrDown ->
            if List.length model.path >= 2 * model.n then
                ( model, Cmd.none )

            else
                let
                    ( ups, downs ) =
                        countUpsDowns model.path
                in
                -- TODO deduplicate the step adding validation logic
                if ups >= model.n then
                    ( { model | path = model.path ++ [ Down ] }, Cmd.none )

                else if downs >= model.n then
                    ( { model | path = model.path ++ [ Up ] }, Cmd.none )

                else
                    ( model, Random.generate identity <| Random.uniform AddUp [ AddDown ] )

        SetN nStr ->
            case Maybe.map (Basics.clamp 1 maxN) <| String.toInt nStr of
                Just newN ->
                    ( { model
                        | n = newN
                        , path = trimPath newN model.path
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        GenerateRandomLatticePath ->
            ( model
            , Random.generate SetPath <|
                Random.List.shuffle (List.repeat model.n Up ++ List.repeat model.n Down)
            )

        GenerateRandomCatalanPath ->
            ( model
            , Random.generate (SetPath << toCatalan) <|
                Random.List.shuffle (List.repeat model.n Up ++ List.repeat model.n Down)
            )

        SetPath newPath ->
            ( { model | path = newPath }, Cmd.none )

        SetSmallestPath ->
            ( { model | path = List.repeat model.n Down ++ List.repeat model.n Up }, Cmd.none )

        SetNextPath ->
            ( { model | path = nextPath model.path }, Cmd.none )


{-| Trim the paths when shrinking the grid size.
Anything that sticks out of the shrunken grid is "bent" into the grid.
-}
trimPath : Int -> Path -> Path
trimPath n path =
    List.reverse <|
        Tuple.second <|
            List.foldl
                (\step ( ( curX, curY ), steps ) ->
                    let
                        nextStep =
                            case step of
                                Up ->
                                    if curX + curY >= 2 * n {- We're at north-east edge -} then
                                        Down

                                    else
                                        Up

                                Down ->
                                    if curX - curY >= 2 * n {- We're at south-east edge -} then
                                        Up

                                    else
                                        Down

                        deltaY =
                            case nextStep of
                                Up ->
                                    1

                                Down ->
                                    -1
                    in
                    ( ( curX + 1, curY + deltaY ), nextStep :: steps )
                )
                ( ( 0, 0 ), [] )
            <|
                List.take (2 * n) path


countUpsDowns : Path -> ( Int, Int )
countUpsDowns =
    List.foldl
        (\step ( ups, downs ) ->
            case step of
                Up ->
                    ( ups + 1, downs )

                Down ->
                    ( ups, downs + 1 )
        )
        ( 0, 0 )


view model =
    { title = "Catalan"
    , body =
        [ viewControls model
        , pathFacts model
        , viewLattice model
        ]
    }


viewControls : Model -> Html Msg
viewControls model =
    Html.div []
        [ Html.label []
            [ Html.text "N = "
            , Html.input
                [ HA.type_ "number"
                , HA.min (String.fromInt minN)
                , HA.max (String.fromInt maxN)
                , HA.step "1"
                , HA.value <| String.fromInt model.n
                , HE.onInput SetN
                ]
                []
            ]
        , Html.button [ HE.onClick GenerateRandomLatticePath ] [ Html.text "Generate random path" ]
        , Html.button [ HE.onClick GenerateRandomCatalanPath ] [ Html.text "Generate random catalan path" ]
        , Html.div []
            [ Html.button [ HE.onClick SetSmallestPath ] [ Html.text "First path" ]
            , Html.button [ HE.onClick SetNextPath ] [ Html.text "Next path" ]
            ]

        -- TODO add text explaining arrow keyboard shortcuts
        ]


pathFacts : Model -> Html msg
pathFacts { path, n } =
    let
        upStepsAfterLastAbsoluteMinimum =
            countUpStepsAfterLastAbsoluteMinimum path
    in
    Html.div []
        [ Html.div []
            [ Html.text <| "There are (2n C n) = " ++ String.fromInt (countLatticePaths n) ++ " lattice paths between (0, 0) and (0, " ++ String.fromInt (2 * n) ++ ")."
            ]
        , Html.div []
            [ Html.text <| "(2n C n) / (n+1) = " ++ String.fromInt (countLatticePaths n // (n + 1)) ++ " of these are catalan paths."
            ]
        , Html.text <|
            if upStepsAfterLastAbsoluteMinimum == 0 then
                "This is a catalan path."

            else
                "This is not a catalan path. It has "
                    ++ String.fromInt upStepsAfterLastAbsoluteMinimum
                    ++ " upsteps after the last absolute minimum."
        ]


{-| 2n C n
-}
countLatticePaths : Int -> Int
countLatticePaths n =
    List.product (List.range (n + 1) (2 * n))
        // List.product (List.range 1 n)


squareSize : Float
squareSize =
    25


minN : Int
minN =
    1


maxN : Int
maxN =
    10


viewLattice : Model -> Html Msg
viewLattice model =
    S.svg
        [ SA.width (px 600)
        , SA.height (px 600)
        ]
        [ S.g
            [ SA.transform
                [ Scale squareSize -squareSize
                , Translate 0 (-1 * toFloat model.n)
                ]
            ]
            [ grid model
            , latticePath Color.red model.path

            --, let
            --    catalanRepresentative =
            --        toCatalan model.path
            --  in
            --  if catalanRepresentative /= model.path then
            --    latticePath Color.lightBlue catalanRepresentative
            --
            --  else
            --    Html.text ""
            ]
        ]


grid : Model -> Svg msg
grid { n } =
    let
        upLines =
            S.g [] <|
                List.map
                    (\i ->
                        S.line
                            [ SA.x1 <| px <| toFloat i
                            , SA.y1 <| px <| toFloat -i
                            , SA.x2 <| px <| toFloat <| n + i
                            , SA.y2 <| px <| toFloat <| n - i
                            ]
                            []
                    )
                <|
                    List.range 0 n

        downLines =
            S.g [] <|
                List.map
                    (\i ->
                        S.line
                            [ SA.x1 <| px <| toFloat i
                            , SA.y1 <| px <| toFloat i
                            , SA.x2 <| px <| toFloat <| n + i
                            , SA.y2 <| px <| toFloat <| -n + i
                            ]
                            []
                    )
                <|
                    List.range 0 n
    in
    S.g
        [ SA.strokeWidth (px 0.05)
        , SA.stroke <| Paint Color.darkGray
        ]
        [ upLines, downLines ]


latticePath : Color.Color -> Path -> Svg msg
latticePath color path =
    S.g
        [ SA.strokeWidth (px 0.1)
        , SA.stroke <| Paint color
        , SA.strokeLinecap StrokeLinecapRound
        ]
    <|
        List.reverse <|
            Tuple.second <|
                List.foldl
                    (\step ( ( curX, curY ), lineSegments ) ->
                        let
                            ( nextX, nextY ) =
                                ( curX + 1
                                , curY
                                    + (case step of
                                        Up ->
                                            1

                                        Down ->
                                            -1
                                      )
                                )

                            nextSegment =
                                S.line
                                    [ SA.x1 <| px curX
                                    , SA.y1 <| px curY
                                    , SA.x2 <| px nextX
                                    , SA.y2 <| px nextY
                                    ]
                                    []
                        in
                        ( ( nextX, nextY ), nextSegment :: lineSegments )
                    )
                    ( ( 0, 0 ), [] )
                    path


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decoder Msg
keyDecoder =
    HE.keyCode
        |> Decode.andThen
            (\keyCode ->
                case keyCode of
                    -- Left arrow
                    37 ->
                        Decode.succeed RemoveLastStep

                    -- Up arrow
                    38 ->
                        Decode.succeed AddUp

                    -- Right arrow
                    39 ->
                        Decode.succeed AddRandomUpOrDown

                    -- Down arrow
                    40 ->
                        Decode.succeed AddDown

                    _ ->
                        Decode.fail ""
            )


toCatalan : Path -> Path
toCatalan p =
    let
        upSteps =
            countUpStepsAfterLastAbsoluteMinimum p
    in
    if upSteps == 0 then
        p

    else
        bufToFub upSteps p


{-| This is the BUF -> FUB transformation mentioned in 52 c.
We find the i-th upstep from the end,
-}
bufToFub : Int -> Path -> Path
bufToFub upStepIndexFromTheEnd p =
    let
        ( b, f ) =
            List.foldr
                (\step ( upStepsWeHave, bSteps, fSteps ) ->
                    if upStepsWeHave == upStepIndexFromTheEnd then
                        ( upStepsWeHave, step :: bSteps, fSteps )

                    else
                        case step of
                            Up ->
                                ( upStepsWeHave + 1, bSteps, Up :: fSteps )

                            Down ->
                                ( upStepsWeHave, bSteps, Down :: fSteps )
                )
                ( 0, [], [] )
                p
                |> (\( _, b0, uf ) -> ( b0, List.tail uf |> Maybe.withDefault [] ))
    in
    f ++ Up :: b


{-| Kenneth P. Bogart: Combinatorics Through Guided Discovery;

Problem 52 gives a simple way to explain why there's `(2n C n) / (n+1)` catalan numbers.
The `n+1` comes from the fact that the `2n C n` lattice paths from (0,0) to (0,2n) can be partitioned
into `n+1` blocks B where paths in block B\_i contain those paths that have i upsteps after the last absolute minimum.

Catalan paths are exactly those in B\_0 and for each i there's a bijection between B\_0 and B\_i given as follows:

Find i-th upsteps U in the catalan path and take F to be everything before that and B everything after that.
Then from this FUB catalan path make BUF which will be a path in B\_i.

-}
countUpStepsAfterLastAbsoluteMinimum : Path -> Int
countUpStepsAfterLastAbsoluteMinimum =
    (\( _, _, x ) -> x)
        << List.foldl
            (\step ( curY, minY, upStepsAfterMinCount ) ->
                case step of
                    Up ->
                        ( curY + 1, minY, upStepsAfterMinCount + 1 )

                    Down ->
                        if curY - 1 <= minY then
                            ( curY - 1, curY - 1, 0 )

                        else
                            ( curY - 1, minY, upStepsAfterMinCount )
            )
            ( 0, 0, 0 )



-- TODO add buttons to iterate through all catalan paths in some orderly fashion


{-| Next path in linear order
-}
nextPath : Path -> Path
nextPath =
    Tuple.second
        << List.foldr
            (\step ( swapDone, tail ) ->
                case ( swapDone, step, tail ) of
                    ( True, _, _ ) ->
                        ( True, step :: tail )

                    ( False, Down, Up :: rest ) ->
                        ( True, Up :: Down :: List.sortBy stepToComparable rest )

                    ( False, _, rest ) ->
                        ( False, step :: rest )
            )
            ( False, [] )


stepToComparable : Step -> Int
stepToComparable step =
    case step of
        Down ->
            0

        Up ->
            1
