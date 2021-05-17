module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import ChungFellerRevisited exposing (countFlaws, fewerFlaws, moreFlaws)
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode exposing (Decoder)
import Path
    exposing
        ( Path
        , Step(..)
        , countUpStepsAfterLastAbsoluteMinimum
        , countUpsDowns
        , nextPath
        , toCatalan
        , trimPath
        )
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
    | FewerFlaws
    | MoreFlaws


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
            ( addStep Up model
            , Cmd.none
            )

        AddDown ->
            ( addStep Down model
            , Cmd.none
            )

        AddRandomUpOrDown ->
            ( model
            , Random.generate identity <| Random.uniform AddUp [ AddDown ]
            )

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

        MoreFlaws ->
            ( { model | path = moreFlaws model.path }, Cmd.none )

        FewerFlaws ->
            ( { model | path = fewerFlaws model.path }, Cmd.none )


addStep : Step -> Model -> Model
addStep step model =
    if List.length model.path >= 2 * model.n then
        model

    else
        let
            ( ups, downs ) =
                countUpsDowns model.path
        in
        if ups >= model.n then
            { model | path = model.path ++ [ Down ] }

        else if downs >= model.n then
            { model | path = model.path ++ [ Up ] }

        else
            { model | path = model.path ++ [ step ] }


view : Model -> Document Msg
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


pathFacts : Model -> Html Msg
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
        , Html.div []
            [ Html.text <|
                if upStepsAfterLastAbsoluteMinimum == 0 then
                    "This is a catalan path."

                else
                    "This is not a catalan path. It has "
                        ++ String.fromInt upStepsAfterLastAbsoluteMinimum
                        ++ " Up steps after the last absolute minimum."
            ]
        , Html.div []
            [ Html.text <| "This path has " ++ String.fromInt (countFlaws path) ++ " flaws "
            , Html.button [ HE.onClick FewerFlaws, HA.disabled (countFlaws path == 0) ] [ Html.text "Fewer flaws" ]
            , Html.button [ HE.onClick MoreFlaws, HA.disabled (countFlaws path >= n) ] [ Html.text "More flaws" ]
            ]
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



-- TODO add buttons to iterate through all catalan paths in some orderly fashion
