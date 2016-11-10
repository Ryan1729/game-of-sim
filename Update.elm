module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (..)
import Extras
import Random.Pcg as Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( Model.defaultModel, Cmd.none )

        Place boardId ->
            case model.selected of
                Just piece ->
                    let
                        newModel =
                            { model
                                | board = Model.place boardId piece model.board
                            }
                    in
                        if isCPULosingModel newModel then
                            ( { newModel | gameState = Win }, Cmd.none )
                        else if isUserLosingModel newModel then
                            ( { newModel | gameState = Loss }, Cmd.none )
                        else
                            ( cpuTurn newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Select piece ->
            ( { model | selected = Just piece }, Cmd.none )


type alias Move =
    ( Piece, BoardId )


getMoves : Board -> List Move
getMoves board =
    Model.getAvailableBoardIds board
        |> List.map ((,) CPU)
        |> Extras.shuffle (Random.initialSeed 42)


isCPULosingModel : Model -> Bool
isCPULosingModel model =
    boardHasTriangleOf CPU model.board


isUserLosingModel : Model -> Bool
isUserLosingModel model =
    boardHasTriangleOf User model.board


boardHasTriangleOf : Piece -> Board -> Bool
boardHasTriangleOf piece board =
    let
        matchingEdges =
            getListOfMatchingEdges piece board
    in
        matchingEdges
            |> Extras.find
                (\( corner1, corner2 ) ->
                    let
                        corner2Targets =
                            List.filterMap
                                (\( source, target ) ->
                                    if source == corner2 then
                                        Just target
                                    else if target == corner2 then
                                        Just source
                                    else
                                        Nothing
                                )
                                matchingEdges
                    in
                        corner2Targets
                            |> List.any
                                (\corner3 ->
                                    List.member ( corner3, corner1 ) matchingEdges
                                        || List.member ( corner1, corner3 ) matchingEdges
                                )
                )
            |> isJust


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


winningMove : Model -> Move -> Bool
winningMove model move =
    applyMove model move
        |> isUserLosingModel


losingMove : Model -> Move -> Bool
losingMove model move =
    applyMove model move
        |> isCPULosingModel


cpuTurn : Model -> Model
cpuTurn model =
    let
        moves : List Move
        moves =
            getMoves model.board

        postMovementModel =
            Extras.find (winningMove model) moves
                |> Extras.orElseLazy (\() -> Extras.find (losingMove model >> not) moves)
                |> Extras.orElseLazy (\() -> Random.step (Random.sample moves) (Random.initialSeed 42) |> fst)
                |> Maybe.map (applyMove model)
                |> Maybe.withDefault model
    in
        if isCPULosingModel postMovementModel then
            { postMovementModel | gameState = Win }
        else if isUserLosingModel postMovementModel then
            { postMovementModel | gameState = Loss }
        else
            postMovementModel


applyMove : Model -> Move -> Model
applyMove model ( piece, boardId ) =
    { model | board = Model.place boardId piece model.board }
