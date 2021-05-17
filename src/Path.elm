module Path exposing
    ( Path
    , Step(..)
    , countUpStepsAfterLastAbsoluteMinimum
    , countUpsDowns
    , nextPath
    , stepToComparable
    , toCatalan
    , trimPath
    )


type Step
    = Down
    | Up


type alias Path =
    List Step


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
            -1

        Up ->
            1
