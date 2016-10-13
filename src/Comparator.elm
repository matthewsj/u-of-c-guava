module Comparator exposing (..)

import List.Extra exposing (dropWhile)

type alias Comparator a =
    a -> a -> Order


naturalComparison : Comparator comparable
naturalComparison x y =
    if x < y then
        LT
    else if x > y then
        GT
    else
        EQ


fromLessThan : (a -> a -> Bool) -> Comparator a
fromLessThan lt x y =
    if lt x y then
        LT
    else if lt y x then
        GT
    else
        EQ


compareFieldWith : Comparator b -> (a -> b) -> Comparator a
compareFieldWith compareField extractField x y =
    compareField (extractField x) (extractField y)


compareField : (a -> comparable) -> Comparator a
compareField =
    compareFieldWith naturalComparison


breakTiesWith : Comparator a -> Comparator a -> Comparator a
breakTiesWith tiebreaker mainComparator x y =
    case mainComparator x y of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            tiebreaker x y


dropFirst : List a -> List a
dropFirst items =
    case items of
        x :: xs ->
            xs

        [] ->
            []


explicitOrdering : List a -> Comparator a
explicitOrdering items =
    let
        explicitLessThan x y =
            dropWhile (\item -> item /= x) items
                |> dropFirst
                |> List.member y
    in
        fromLessThan explicitLessThan
