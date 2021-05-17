module ChungFellerRevisited exposing
    ( countFlaws
    , fewerFlaws
    , moreFlaws
    )

import List.Extra as List
import Path exposing (Path, Step(..), stepToComparable)


{-| From "The Chung–Feller theorem revisited" <https://core.ac.uk/download/pdf/81931391.pdf>

An n-Dyck path with k flaws is a path from (0, 0) to (2n, 0) with up (1, 1) and down (1, −1) steps having k down steps below the x-axis.

-}
countFlaws : Path -> Int
countFlaws =
    Tuple.second
        << List.foldl
            (\step ( curY, flawCount ) ->
                case step of
                    Down ->
                        ( curY - 1
                        , flawCount
                            + (if curY <= 0 then
                                1

                               else
                                0
                              )
                        )

                    Up ->
                        ( curY + 1, flawCount )
            )
            ( 0, 0 )


{-| Map path with k flaws to the map with (k+1) (if not already at the most flawed).

1.  Find decomposition BuAdC, where u is the first up step above x axis,
    and d is the first down step touching x-axis after u.
2.  AdBuC is the desired result

-}
moreFlaws : Path -> Path
moreFlaws p =
    List.zip p (depths p)
        |> List.span (\( _, depth ) -> depth < 1)
        |> (\( b, uAdC ) ->
                {- drop u -}
                List.tail uAdC
                    |> Maybe.andThen
                        (\adC ->
                            List.span (\( _, depth ) -> depth > 0) adC
                                |> (\( a, dc ) ->
                                        {- drop d -}
                                        List.tail dc
                                            |> Maybe.map
                                                (\c ->
                                                    List.map Tuple.first a
                                                        ++ Down
                                                        :: List.map Tuple.first b
                                                        ++ Up
                                                        :: List.map Tuple.first c
                                                )
                                   )
                        )
           )
        |> Maybe.withDefault p


{-| Inverse of moreFlaws
-}
fewerFlaws : Path -> Path
fewerFlaws p =
    List.zip p (depths p)
        |> List.span (\( _, depth ) -> depth > -1)
        |> (\( a, dBuC ) ->
                {- drop d -}
                List.tail dBuC
                    |> Maybe.andThen
                        (\buC ->
                            List.span (\( _, depth ) -> depth < 0) buC
                                |> (\( b, uC ) ->
                                        {- drop u -}
                                        List.tail uC
                                            |> Maybe.map
                                                (\c ->
                                                    List.map Tuple.first b
                                                        ++ Up
                                                        :: List.map Tuple.first a
                                                        ++ Down
                                                        :: List.map Tuple.first c
                                                )
                                   )
                        )
           )
        |> Maybe.withDefault p


depths : Path -> List Int
depths =
    List.scanl (\step acc -> stepToComparable step + acc) 0
        >> List.tail
        >> Maybe.withDefault []
