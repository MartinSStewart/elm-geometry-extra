module Tests.Point2d exposing (tests)

import Expect
import Point2d exposing (Point2d)
import Point2d.Extra as Point2d
import Quantity
import Random
import Test exposing (Test)


minPointFirst : ( Point2d u c, Point2d u c ) -> ( Point2d u c, Point2d u c )
minPointFirst ( a, b ) =
    if Point2d.xCoordinate a |> Quantity.lessThan (Point2d.xCoordinate b) then
        ( a, b )

    else
        ( b, a )


tests : Test
tests =
    Test.test "Get closest" <|
        \_ ->
            let
                {- We don't use a fuzzer here because it will generate cases where two pairs of points are exactly
                   the same distance apart which will lead to false negative test results
                -}
                generator : Random.Generator (List (Point2d u c))
                generator =
                    Random.list 50 (Random.map2 (\x y -> Point2d.unsafe { x = x, y = y }) (Random.float -5 5) (Random.float -5 5))

                points : List (Point2d u c)
                points =
                    Random.step generator (Random.initialSeed 123) |> Tuple.first

                actual : Maybe ( Point2d u c, Point2d u c )
                actual =
                    Point2d.nearestPairBy identity points |> Maybe.map minPointFirst

                expected : Maybe ( Point2d u c, Point2d u c )
                expected =
                    bruteForceNearest identity points |> Maybe.map minPointFirst
            in
            Expect.equal expected actual


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
