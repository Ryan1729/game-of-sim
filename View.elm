module View exposing (view)

import Model exposing (..)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events
import Msg exposing (Msg(..))
import Svg exposing (Svg, svg, rect, path, circle, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ Html.Attributes.style
                [ ( "width", boardWidthString ++ "px" )
                , ( "display", "flex" )
                , ( "justify-content", "center" )
                , ( "font-size", (boardWidth / 32 |> toString) ++ "px" )
                , ( "flex-wrap", "wrap" )
                ]
            ]
            [ model.gameState
                |> gameStateToString
                |> Html.text
            , svg
                [ width boardWidthString
                , height boardHeightString
                , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                ]
                [ renderBoard model.selected model.board
                ]
            ]
        ]


gameStateToString : GameState -> String
gameStateToString gameState =
    case gameState of
        Win ->
            "You won!"

        Loss ->
            "You lost!"

        _ ->
            ""


nodes =
    Model.nodeIdPossibilities
        |> List.map nodeIdToPosition
        |> List.map drawNode


drawNode : ( Float, Float ) -> Svg Msg
drawNode ( xPos, yPos ) =
    circle [ cx (toString xPos), cy (toString yPos), r "40", fill "#2ECC40" ] []


renderBoard : Maybe Piece -> Board -> Svg Msg
renderBoard selected board =
    renderEdges selected board
        ++ nodes
        |> g []


renderEdges : Maybe Piece -> Board -> List (Svg Msg)
renderEdges selected board =
    Model.boardIdPossibilities
        |> List.map
            (\boardId ->
                case getPiece boardId board of
                    Just piece ->
                        drawEdge (attributesFromPiece piece) boardId

                    Nothing ->
                        case selected of
                            Just _ ->
                                drawEdge [ onClick (Place boardId), stroke "white", strokeWidth "2", fill "black", fillOpacity "0.4" ] boardId

                            Nothing ->
                                drawEdge [ fill "black", fillOpacity "0.4" ] boardId
            )


attributesFromPiece : Piece -> List (Svg.Attribute Msg)
attributesFromPiece piece =
    case piece of
        User ->
            [ fill "#0074D9" ]

        CPU ->
            [ fill "#FF851B" ]


halfEdgeSize =
    5


drawEdge : List (Svg.Attribute Msg) -> BoardId -> Svg Msg
drawEdge extraAttributes ( source, target ) =
    let
        ( x1, y1 ) =
            nodeIdToPosition source

        ( x2, y2 ) =
            nodeIdToPosition target

        yOffset =
            --without this, then the lines are different thicknesses
            --depending on direction
            if x1 < x2 || y1 > y2 then
                -halfEdgeSize
            else
                halfEdgeSize

        dString =
            "M "
                ++ toString (x1 - halfEdgeSize)
                ++ " "
                ++ toString (y1 - yOffset)
                ++ " L "
                ++ toString (x1 + halfEdgeSize)
                ++ " "
                ++ toString (y1 + yOffset)
                ++ " L "
                ++ toString (x2 + halfEdgeSize)
                ++ " "
                ++ toString (y2 + yOffset)
                ++ " L "
                ++ toString (x2 - halfEdgeSize)
                ++ " "
                ++ toString (y2 - yOffset)
                ++ " Z"
    in
        Svg.path (extraAttributes ++ [ d dString ]) []


nodeIdToPosition : NodeId -> ( Float, Float )
nodeIdToPosition nodeId =
    case nodeId of
        A ->
            ( boardWidth * 5 / 6, centerY )

        B ->
            ( boardWidth * 2 / 3, boardHeight / 6 )

        C ->
            ( boardWidth / 3, boardHeight / 6 )

        D ->
            ( boardWidth / 6, centerY )

        E ->
            ( boardWidth / 3, boardHeight * 5 / 6 )

        F ->
            ( boardWidth * 2 / 3, boardHeight * 5 / 6 )


boardWidth =
    720


boardWidthString =
    toString boardWidth


boardHeight =
    720


boardHeightString =
    toString boardHeight


centerX =
    boardWidth / 2


centerY =
    boardHeight / 2
