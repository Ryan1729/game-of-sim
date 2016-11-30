module Model exposing (..)

import EveryDict exposing (EveryDict)
import Extras


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , gameState : GameState
    }


defaultModel =
    { board = initialBoard
    , selected = Just User
    , gameState = InProgress
    }


type GameState
    = InProgress
    | Win
    | Loss


type alias Board =
    EveryDict BoardId Piece


getPiece =
    EveryDict.get


initialBoard : Board
initialBoard =
    EveryDict.empty


boardIdPossibilities =
    nodeIdPossibilities
        |> List.concatMap
            (\nodeId1 ->
                List.filterMap
                    (\nodeId2 ->
                        case nodeIdComparer nodeId1 nodeId2 of
                            LT ->
                                Just ( nodeId1, nodeId2 )

                            _ ->
                                Nothing
                    )
                    nodeIdPossibilities
            )


getListOfMatchingEdges : Piece -> Board -> List BoardId
getListOfMatchingEdges piece board =
    EveryDict.toList board
        |> List.filterMap
            (\( boardId, edgePiece ) ->
                if piece == edgePiece then
                    Just boardId
                else
                    Nothing
            )


type Piece
    = User
    | CPU


type alias BoardId =
    ( NodeId, NodeId )


boardIdComparer ( nodeId1, nodeId2 ) ( nodeId3, nodeId4 ) =
    case nodeIdComparer nodeId1 nodeId3 of
        EQ ->
            nodeIdComparer nodeId2 nodeId4

        otherwise ->
            otherwise


type NodeId
    = A
    | B
    | C
    | D
    | E
    | F


nodeIdPossibilities =
    [ A
    , B
    , C
    , D
    , E
    , F
    ]


nodeIdComparer nodeId1 nodeId2 =
    compare (Extras.indexOfDefault nodeIdPossibilities nodeId1)
        (Extras.indexOfDefault nodeIdPossibilities nodeId2)


place : BoardId -> Piece -> Board -> Board
place =
    EveryDict.insert


getAvailableBoardIds : Board -> List BoardId
getAvailableBoardIds board =
    let
        usedIds =
            EveryDict.keys board
    in
        boardIdPossibilities
            |> List.filter (\boardId -> List.member boardId usedIds |> not)
