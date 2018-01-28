{- Copyright (C) 2018 Nicholas Hansen
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3 or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/ -}
module Sankey exposing (..)

import Dict exposing (Dict)
import Graph exposing (Graph, AcyclicGraph)
import Html exposing (Html)
import IntDict exposing (IntDict)
import List
import Maybe exposing (withDefault)
import State exposing (State)
import String
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Tuple

------------
-- CONFIG --
------------

type alias Options =
    { initializeX : Int -> Float
    , initializeY : Int -> Float
    , verticalPadding : Float
    , horizontalPadding : Float
    , nodeWidth : Float
    , svgWidth : Int
    , svgHeight : Int
    , iterations : Int}

defaults : Options
defaults = { initializeX = (\x -> toFloat x * 50 )
           , initializeY = (\y -> toFloat y * 50 )
           , verticalPadding = 20
           , horizontalPadding = 20
           , nodeWidth = 25
           , svgWidth = 800
           , svgHeight = 800
           , iterations = 5 }

-----------
-- TYPES --
-----------

type alias Node a =
    { label : a
    , x : Float
    , y : Float
    , throughput : Float }

type alias Edge =
    { startX : Float
    , startY : Float
    , endX : Float
    , endY : Float
    , throughput : Float }

type alias Diagram a =
    { nodes : List (Node a)
    , edges : List Edge }

type alias Inputs a =
    { nodes : List (Graph.Node a)
    , flows : List (Graph.Edge Float) }

type alias NodeContext a = Graph.NodeContext a Float

type alias LayoutColumn a = List (NodeContext a)

type alias Layout a = List (LayoutColumn a)

type alias ProcessorState a b = State (IntDict (Node a)) b

-----------------------
-- UTILITY FUNCTIONS --
-----------------------

fromContext : (Node a -> x)
            -> x
            -> IntDict (Node a)
            -> NodeContext a
            -> x
fromContext f default nodeDict ctx =
    IntDict.get ctx.node.id nodeDict
        |> Maybe.map f
        |> withDefault default

nodeCenter : Node a -> Float
nodeCenter node = node.y + node.throughput / 2

weightedAverage : List (Float,Float) -> Float
weightedAverage vals =
    case List.foldl (\(value,weight) (numerator,denominator) -> (numerator+weight*value,denominator+weight)) (0,0) vals of
        (_, 0) -> 0
        (numerator,denominator) -> numerator / denominator

------------------------
-- DIAGRAM GENERATION --
------------------------

generateLayout : Inputs a -> Layout a
generateLayout inputs =
    Graph.fromNodesAndEdges inputs.nodes inputs.flows
        |> Graph.checkAcyclic
        |> Result.map Graph.heightLevels
        |> Result.withDefault [] -- TODO error handling

decorateNodes : List (Graph.Node a) -> IntDict (Node a)
decorateNodes = List.map (\{id, label} -> (id, { label = label
                                               , throughput = 0
                                               , x = 0
                                               , y = 0 }))
                >> IntDict.fromList

traverseColumn : (Int -> Int -> NodeContext a -> s -> s)
               -> Int
               -> LayoutColumn a
               -> State s (LayoutColumn a)
traverseColumn operation colIx column =
    column
        |> List.indexedMap (\rowIx ctx -> (colIx, rowIx, ctx))
        |> State.traverse
           (\(colIx, rowIx, ctx) -> State.modify (operation colIx rowIx ctx)
           |> State.andThen (\_ -> State.state ctx))

traverseColumns : ((Int, LayoutColumn a) -> State s (LayoutColumn a))
                -> Layout a
                -> State s (Layout a)
traverseColumns operation layout =
    layout
        |> List.indexedMap (,)
        |> State.traverse operation

traverseLayout : (Int -> Int -> NodeContext a -> s -> s)
               -> Layout a
               -> State s (Layout a)
traverseLayout operation layout =
    traverseColumns
        (\(i,column) -> traverseColumn operation i column)
        layout

initializeNodes : Options -> Layout a -> ProcessorState a (Layout a)
initializeNodes opts =
    traverseLayout (\colIx rowIx ctx ->
                        IntDict.update ctx.node.id
                        (Maybe.map (\node -> { node
                                                 | y = opts.initializeX rowIx
                                                 , x = opts.initializeY colIx
                                                 , throughput =
                                                   max
                                                   ( ctx.incoming
                                                   |> IntDict.values
                                                   |> List.sum )
                                                   (ctx.outgoing
                                                   |> IntDict.values
                                                   |> List.sum )})))

sortColumn : LayoutColumn a -> ProcessorState a (LayoutColumn a)
sortColumn column =
    State.get |> State.andThen ( \nodeDict ->
                                     column
                                         |> List.sortBy (fromContext .y 0 nodeDict)
                                         |> State.state )

pullEdges : Options
          -> (Int, LayoutColumn a)
          -> ProcessorState a (LayoutColumn a)
pullEdges opts (colIx, column) =
    let
        handleNode : Int -> Int -> NodeContext a -> IntDict (Node a) -> IntDict (Node a)
        handleNode _ _ ctx nodeDict =
            IntDict.update ctx.node.id
                (Maybe.map (\node -> { node
                                         | y = IntDict.toList ctx.incoming
                                                   |> List.append (IntDict.toList ctx.outgoing)
                                                   |> List.map (Tuple.mapFirst (\id ->
                                                                                    IntDict.get id nodeDict
                                                                                        |> Maybe.map nodeCenter
                                                                                        |> withDefault 0))
                                                   |> weightedAverage
                                                   |> (\y -> y - node.throughput / 2)}))
                nodeDict
    in
        traverseColumn handleNode colIx column

resolveOverlaps : Options -> LayoutColumn a -> ProcessorState a (LayoutColumn a)
resolveOverlaps opts column =
    let
        pushDown : Float -> NodeContext a -> ProcessorState a Float
        pushDown prevBound ctx =
            State.advance (\nodeDict ->
                               case IntDict.get ctx.node.id nodeDict of
                                   Nothing -> (prevBound,nodeDict)
                                   Just node ->
                                       let
                                           newY = max node.y prevBound
                                       in
                                           ( newY + node.throughput + opts.verticalPadding
                                           , IntDict.insert ctx.node.id {node | y = newY} nodeDict ))

        pushUp : Float -> NodeContext a -> ProcessorState a Float
        pushUp prevBound ctx =
            State.advance (\nodeDict ->
                               case IntDict.get ctx.node.id nodeDict of
                                   Nothing -> (prevBound,nodeDict)
                                   Just node ->
                                       let
                                           newY = min node.y (prevBound - node.throughput - opts.verticalPadding)
                                       in
                                           ( newY
                                           , IntDict.insert ctx.node.id {node | y = newY} nodeDict ))
    in
        State.foldlM pushDown 0 column
            |> State.andThen (\_ -> State.state column)

relaxNodes : Options
           -> Layout a
           -> ProcessorState a (Layout a)
relaxNodes opts =
    let
        relaxColumn : (Int, LayoutColumn a) -> ProcessorState a (LayoutColumn a)
        relaxColumn col =
            pullEdges opts col
                |> State.andThen (\col -> sortColumn col)
                |> State.andThen (\col -> resolveOverlaps opts col)

        relaxLayout : Layout a -> ProcessorState a (Layout a)
        relaxLayout layout =
            traverseColumns relaxColumn layout
                |> State.map List.reverse
                |> State.andThen (\l -> traverseColumns relaxColumn l)
                |> State.map List.reverse

        go : Int -> Layout a -> ProcessorState a (Layout a)
        go i layout =
            if i >= opts.iterations
            then
                State.state layout
            else
                relaxLayout layout
                    |> State.andThen (go (i+1))

    in
        go 0

outputDiagram : Options -> Inputs a -> Layout a -> ProcessorState a (Diagram a)
outputDiagram opts inputs layout =
    State.embed (\nodeDict ->
                      { nodes = IntDict.values nodeDict
                      , edges =
                          let
                              edgeKey x y = (max x y, min x y)

                              updateEdges : Int -> Int -> NodeContext a -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
                              updateEdges _ _ ctx = placeIncomingEdges ctx >> placeOutgoingEdges ctx

                              placeIncomingEdges : NodeContext a -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
                              placeIncomingEdges ctx edgeDict =
                                  let
                                      x = fromContext .x 0 nodeDict ctx
                                  in
                                      IntDict.toList ctx.incoming
                                          |> List.sortBy (\(id,_) ->
                                                              IntDict.get id nodeDict
                                                                  |> Maybe.map .y
                                                                  |> withDefault 0)
                                          |> List.foldl (\(id,flow) (edgeDict,y) ->
                                                             ( Dict.update (edgeKey id ctx.node.id)
                                                                   (\e -> case e of
                                                                              Just edge -> Just {edge
                                                                                                    | endY = y
                                                                                                    , endX = x}
                                                                              Nothing -> Just { endY = y
                                                                                              , endX = x
                                                                                              , throughput = flow
                                                                                              , startX = 0
                                                                                              , startY = 0})
                                                                   edgeDict
                                                             , y + flow))
                                             (edgeDict, fromContext .y 0 nodeDict ctx)
                                          |> Tuple.first

                              placeOutgoingEdges : NodeContext a -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
                              placeOutgoingEdges ctx edgeDict =
                                  let
                                      x = fromContext .x 0 nodeDict ctx + opts.nodeWidth
                                  in
                                      IntDict.toList ctx.outgoing
                                          |> List.sortBy (\(id,_) ->
                                                              IntDict.get id nodeDict
                                                                  |> Maybe.map .y
                                                                  |> withDefault 0)
                                          |> List.foldl (\(id,flow) (edgeDict,y) ->
                                                             ( Dict.update (edgeKey id ctx.node.id)
                                                                   (\e -> case e of
                                                                              Just edge -> Just {edge
                                                                                                    | startY = y
                                                                                                    , startX = x}
                                                                              Nothing -> Just { startY = y
                                                                                              , startX = x
                                                                                              , throughput = flow
                                                                                              , endX = 0
                                                                                              , endY = 0})
                                                                   edgeDict
                                                             , y + flow))
                                             (edgeDict, fromContext .y 0 nodeDict ctx)
                                          |> Tuple.first
                          in
                              State.finalState Dict.empty (traverseLayout updateEdges layout)
                                  |> Dict.values})


generateDiagram : Options -> Inputs a -> Diagram a
generateDiagram opts inputs =
    generateLayout inputs
        |> initializeNodes opts
        |> State.andThen (relaxNodes opts)
        |> State.andThen (outputDiagram opts inputs)
        |> State.finalValue (decorateNodes inputs.nodes)


---------------
-- RENDERING --
---------------

computeViewBox : Options -> List (Node a) -> String
computeViewBox opts nodes =
    let
        bounds {x,y,throughput} = ( x - opts.horizontalPadding
                                  , y - opts.verticalPadding
                                  , x + opts.nodeWidth + opts.horizontalPadding
                                  , y + throughput + opts.verticalPadding )

        mergeBounds (minX1, minY1, maxX1, maxY1) (minX2, minY2, maxX2, maxY2) =
            ( min minX1 minX2
            , min minY1 minY2
            , max maxX1 maxX2
            , max maxY1 maxY2 )
    in
        case nodes of
            [] -> "0 0 0 0"
            node::rest -> List.foldl (bounds >> mergeBounds) (bounds node) rest
                          |> (\(minX,minY,maxX,maxY) -> [minX,minY,maxX-minX,maxY-minY])
                          |> List.map toString
                          |> String.join " "


render : Options -> Diagram a -> Html msg
render opts {nodes, edges} =
    let
        renderNode : Node a -> Svg msg
        renderNode {label, x, y, throughput} =
            Svg.g []
                [ Svg.rect
                      [ Attr.x <| toString x
                      , Attr.y <| toString y
                      , Attr.width <| toString opts.nodeWidth
                      , Attr.height <| toString throughput
                      , Attr.fill "blue"
                      , Attr.stroke "black"
                      , Attr.strokeWidth "1" ]
                      []
                , Svg.text_
                    [ Attr.x <| toString x
                    , Attr.y << toString <| y + throughput / 2
                    , Attr.fill "white"
                    , Attr.fontSize "9"]
                      [ Svg.text <| toString label ]]

        renderEdge : Edge -> Svg msg
        renderEdge {startX, startY, endX, endY, throughput} =
            let
                startControlX = toString <| (2 * startX + endX) / 3
                endControlX = toString <| (startX + 2 * endX) / 3
                startLowerY = toString <| startY + throughput
                endLowerY = toString <| endY + throughput
                startX_ = toString startX
                startY_ = toString startY
                endX_ = toString endX
                endY_ = toString endY
            in
                Svg.path
                    [ Attr.fill "orange"
                    , Attr.stroke "black"
                    , Attr.strokeWidth "1"
                    , Attr.d <| String.join " " [ "M", startX_, startY_
                                                , "C", startControlX, startY_
                                                , endControlX, endY_
                                                , endX_, endY_
                                                , "L", endX_, endLowerY
                                                , "C", endControlX, endLowerY
                                                , startControlX, startLowerY
                                                , startX_, startLowerY ]]
                    []
    in
        Svg.svg
            [ Attr.width <| toString opts.svgWidth
            , Attr.height <| toString opts.svgHeight
            , Attr.viewBox <| computeViewBox opts nodes
            , Attr.preserveAspectRatio "meet midXmidY"]
            [ Svg.g [] (List.map renderNode nodes)
            , Svg.g [] (List.map renderEdge edges) ]
