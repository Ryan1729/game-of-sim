module Extras exposing (..)

import Random.Pcg as Random exposing (Seed)


shuffle : Seed -> List a -> List a
shuffle seed list =
    let
        length =
            List.length list

        randomTags =
            Random.step (Random.list length (Random.int 0 length)) seed
                |> Tuple.first
    in
        List.map2 (,) randomTags list |> List.sortBy Tuple.first |> List.unzip |> Tuple.second



-- from https://github.com/elm-community/list-extra


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest



-- from https://github.com/elm-community/maybe-extra


orElseLazy : (() -> Maybe a) -> Maybe a -> Maybe a
orElseLazy fma mb =
    case mb of
        Nothing ->
            fma ()

        Just _ ->
            mb



-- derived from http://stackoverflow.com/a/33625733/4496839
-- has less helper functions than the List.Extra version


indexOfhelper : List a -> a -> Int -> Maybe Int
indexOfhelper lst elem offset =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            if x == elem then
                Just offset
            else
                indexOfhelper xs elem (offset + 1)


indexOf : List a -> a -> Maybe Int
indexOf lst element =
    indexOfhelper lst element 0



-- A lot of the time this default is fine


indexOfDefault : List a -> a -> Int
indexOfDefault lst element =
    indexOfhelper lst element 0
        |> Maybe.withDefault -1
