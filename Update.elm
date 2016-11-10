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
                                , rack =
                                    Model.removeFromRack piece model.rack
                                    -- , selected = Nothing
                            }
                    in
                        if isWinningModel newModel then
                            ( { newModel | gameState = Win }, Cmd.none )
                        else if isLosingModel newModel then
                            ( { newModel | gameState = Loss }, Cmd.none )
                        else
                            ( cpuTurn newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Select piece ->
            ( { model | selected = Just piece }, Cmd.none )


type alias Move =
    ( Piece, BoardId )


getMoves : Rack -> Board -> List Move
getMoves rack board =
    List.map2 (,)
        (Model.getAvailablePieces rack)
        (Model.getAvailableBoardIds board)
        |> Extras.shuffle (Random.initialSeed 42)


isWinningModel : Model -> Bool
isWinningModel model =
    boardHasTriangleOf CPU model.board


isLosingModel : Model -> Bool
isLosingModel model =
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


nonLosingMove : Model -> Move -> Bool
nonLosingMove model move =
    let
        potentialModel =
            applyMove model move

        potentialFutureMoves =
            getMoves model.rack potentialModel.board
    in
        case Extras.find (winningMove potentialModel) potentialFutureMoves of
            Just _ ->
                False

            Nothing ->
                True


winningMove : Model -> Move -> Bool
winningMove model move =
    applyMove model move
        |> isWinningModel


cpuTurn : Model -> Model
cpuTurn model =
    let
        moves : List Move
        moves =
            getMoves model.rack model.board
    in
        Extras.find (winningMove model) moves
            |> Extras.orElseLazy (\() -> Extras.find (nonLosingMove model) moves)
            |> Extras.orElseLazy (\() -> Random.step (Random.sample moves) (Random.initialSeed 42) |> fst)
            |> Maybe.map (applyMove model)
            |> Maybe.withDefault model


applyMove : Model -> Move -> Model
applyMove model ( piece, boardId ) =
    { model | board = Model.place boardId piece model.board }
