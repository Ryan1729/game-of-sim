module Model exposing (..)

import GenericDict exposing (GenericDict)
import Extras


type alias Model =
    { board : Board
    , selected : Maybe Piece
    , rack : Rack
    , gameState : GameState
    }


defaultModel =
    { board = initialBoard
    , selected = Just User
    , rack = initialRack
    , gameState = InProgress
    }


type GameState
    = InProgress
    | Win
    | Loss


type alias Board =
    GenericDict BoardId Piece


getPiece =
    GenericDict.get


initialBoard : Board
initialBoard =
    GenericDict.empty boardIdComparer


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
    GenericDict.toList board
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


type alias Rack =
    {}


initialRack : Rack
initialRack =
    {}


removeFromRack : Piece -> Rack -> Rack
removeFromRack piece rack =
    rack


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
    GenericDict.insert


getAvailableBoardIds : Board -> List BoardId
getAvailableBoardIds board =
    let
        usedIds =
            GenericDict.keys board
    in
        boardIdPossibilities
            |> List.filter (\boardId -> List.member boardId usedIds |> not)


getAvailablePieces : Rack -> List Piece
getAvailablePieces rack =
    [ CPU ]
