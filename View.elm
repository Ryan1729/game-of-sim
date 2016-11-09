module View exposing (view)

import MaterialModel exposing (MaterialModel)
import Model exposing (..)
import Html exposing (Html, text)
import Html.App
import Html.Attributes
import MaterialMsg exposing (MaterialMsg(Mdl, U))
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, rect, path, circle, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import PieceView


view : MaterialModel -> Html MaterialMsg
view { mdl, model } =
    Html.div []
        [ Button.render Mdl
            [ 0 ]
            mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick (U NewGame)
            ]
            [ text "New Game" ]
        , Grid.grid []
            [ Grid.cell [ Grid.size All 5 ]
                [ PieceView.renderRack model.selected model.rack
                ]
            , Grid.cell [ Grid.size All 6 ]
                [ Html.div [ Html.Attributes.style [ ( "width", boardWidthString ++ "px" ), ( "display", "flex" ), ( "justify-content", "center" ), ( "font-size", (boardWidth / 32 |> toString) ++ "px" ) ] ]
                    [ model.gameState
                        |> gameStateToString
                        |> Html.text
                    ]
                , svg
                    [ width boardWidthString
                    , height boardHeightString
                    , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                    ]
                    [ renderBoard model.selected model.board
                    ]
                ]
            ]
            |> Html.App.map U
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
    nodes
        ++ renderEdges selected board
        |> g []


renderEdges : Maybe Piece -> Board -> List (Svg Msg)
renderEdges selected board =
    -- let
    --   edges = getEdges board
    -- in
    []


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
