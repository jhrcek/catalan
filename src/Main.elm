module Main exposing (main)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Random
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
    = Up
    | Down


type alias Path =
    List Step


type Msg
    = AddUp
    | AddDown
    | AddRandomUpOrDown
    | RemoveLastStep
    | SetN String


init () =
    ( { n = 10
      , path = [ Up, Up, Down, Up, Up, Up, Down, Up, Down, Down ]
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
                if ups >= model.n then
                    ( { model | path = model.path ++ [ Down ] }, Cmd.none )

                else if downs >= model.n then
                    ( { model | path = model.path ++ [ Up ] }, Cmd.none )

                else
                    ( model, Random.generate identity <| Random.uniform AddUp [ AddDown ] )

        SetN nStr ->
            case String.toInt nStr of
                Just newN ->
                    ( { model
                        | n = newN
                        , path = trimPath newN model.path
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


trimPath : Int -> Path -> Path
trimPath n path =
    List.reverse <|
        Tuple.second <|
            List.foldl
                (\step ( ( curX, curY ), steps ) ->
                    case step of
                        Up ->
                            if curX + curY >= 2 * n {- We're at north-east edge -} then
                                ( ( curX + 1, curY - 1 ), Down :: steps )

                            else
                                ( ( curX + 1, curY + 1 ), Up :: steps )

                        Down ->
                            if curX - curY >= 2 * n {- We're at south-east edge -} then
                                ( ( curX + 1, curY + 1 ), Up :: steps )

                            else
                                ( ( curX + 1, curY - 1 ), Down :: steps )
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
        , viewLattice model
        ]
    }


viewControls : Model -> Html Msg
viewControls model =
    Html.div []
        [ Html.label []
            [ Html.text <| "N = " ++ String.fromInt model.n ++ " "
            , Html.input
                [ HA.type_ "range"
                , HA.min "1"
                , HA.max "10"
                , HA.value (String.fromInt model.n)
                , Events.onInput SetN
                ]
                []
            ]
        , Html.button [] [ Html.text "Random path" ]
        , Html.button [] [ Html.text "Random Catalan Path" ]
        , Html.text "Use Up/Down arrow keys to add new path segments, Right arrow key to add new random segment and Left arrow to remove last segment"
        ]


squareSize : Float
squareSize =
    25


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
            , latticePath model
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
                            , SA.y1 <| px <| toFloat <| -i
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


latticePath : Model -> Svg msg
latticePath { path } =
    S.g
        [ SA.strokeWidth (px 0.1)
        , SA.stroke <| Paint Color.red
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
    Events.keyCode
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
