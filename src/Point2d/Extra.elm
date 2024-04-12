module Point2d.Extra exposing (nearestPair, nearestPairBy)

import List exposing (sortBy)
import Maybe
import Point2d exposing (Point2d)
import Quantity


x : Point2d units coordinates -> Float
x p =
    Point2d.unwrap p |> .x


y : Point2d units coordinates -> Float
y p =
    Point2d.unwrap p |> .y


{-| Find the nearest pair of points in a list a points. Runs in O(n\*log(n)) time.

    import Point2d.Extra exposing (nearestPair)

    point x y = Point2d.unsafe { x = x, y = y }

    nearestPair [ point 0 0, point 100 0, point 1 0 ] --Just (point 0 0, point 0 1)
    nearestPair [ point 0 0 ] --Nothing
    nearestPair [] --Nothing

-}
nearestPair : List (Point2d u c) -> Maybe ( Point2d u c, Point2d u c )
nearestPair pairs =
    nearestPairBy identity pairs


{-| Find the nearest pair of points in a list of values. Runs in O(n\*log(n)) time.

    import Point2d.Extra exposing (nearestPairBy)

    point (x, y) = Point2d.unsafe { x = x, y = y }

    nearestPairBy point [ (0, 0), (100, 0), (1, 0) ] --Just (point (0, 0), point (0, 1))
    nearestPairBy point [ (0, 0) ] --Nothing
    nearestPairBy point [] --Nothing

The original implementation was [found here](https://www.markhneedham.com/blog/2012/05/09/haskell-closest-pairs-algorithm/) and then converted to Elm with the help of GPT 3 turbo

-}
nearestPairBy : (a -> Point2d u c) -> List a -> Maybe ( a, a )
nearestPairBy dataToPoint pairs =
    if List.length pairs <= 3 then
        bruteForceNearest dataToPoint pairs

    else
        let
            sortedByX : List a
            sortedByX =
                sortBy (\a -> dataToPoint a |> x) pairs

            ( leftByX, rightByX ) =
                splitAt (List.length sortedByX // 2) sortedByX
        in
        case ( nearestPairBy dataToPoint leftByX, nearestPairBy dataToPoint rightByX ) of
            ( Just closestLeftPair, Just closestRightPair ) ->
                let
                    closestPair : ( a, a )
                    closestPair =
                        if distance dataToPoint closestLeftPair < distance dataToPoint closestRightPair then
                            closestLeftPair

                        else
                            closestRightPair

                    pairsWithinMinimumDelta : List a
                    pairsWithinMinimumDelta =
                        sortBy (\a -> dataToPoint a |> y) <|
                            List.filter withinMinimumDelta sortedByX

                    withinMinimumDelta : a -> Bool
                    withinMinimumDelta a =
                        abs (xMidPoint - x (dataToPoint a)) <= distance dataToPoint closestPair

                    xMidPoint : Float
                    xMidPoint =
                        case List.head (List.reverse leftByX) of
                            Just a ->
                                dataToPoint a |> x

                            Nothing ->
                                0
                in
                List.foldl
                    (\( p1, p2 ) ( p3, p4 ) ->
                        if distance dataToPoint ( p1, p2 ) < distance dataToPoint ( p3, p4 ) then
                            ( p1, p2 )

                        else
                            ( p3, p4 )
                    )
                    closestPair
                    (pairwise pairsWithinMinimumDelta)
                    |> Just

            _ ->
                Nothing


pairwise : List b -> List ( b, b )
pairwise list =
    case list of
        first :: second :: rest ->
            List.foldl
                (\item { pairs, previous } ->
                    { pairs = ( previous, item ) :: pairs
                    , previous = item
                    }
                )
                { pairs = [ ( first, second ) ], previous = second }
                rest
                |> .pairs
                |> List.reverse

        _ ->
            []


bruteForceNearest : (a -> Point2d u c) -> List a -> Maybe ( a, a )
bruteForceNearest dataToPoint pairs =
    uniquePairs pairs |> minimumBy (distance dataToPoint)


distance : (a -> Point2d u c) -> ( a, a ) -> Float
distance dataToPoint ( a, b ) =
    Point2d.distanceFrom (dataToPoint a) (dataToPoint b) |> Quantity.unwrap


uniquePairs : List a -> List ( a, a )
uniquePairs xs =
    case xs of
        [] ->
            []

        x2 :: xs_ ->
            List.map (\y2 -> ( x2, y2 )) xs_ ++ uniquePairs xs_


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    let
        minBy : a -> ( a, comparable ) -> ( a, comparable )
        minBy x2 (( _, fy ) as min) =
            let
                fx : comparable
                fx =
                    f x2
            in
            if fx < fy then
                ( x2, fx )

            else
                min
    in
    case ls of
        [ l_ ] ->
            Just l_

        l_ :: ls_ ->
            Just <| Tuple.first <| List.foldl minBy ( l_, f l_ ) ls_

        _ ->
            Nothing


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )
