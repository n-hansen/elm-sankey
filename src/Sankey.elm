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
    , edgeElasticity : Float -> Float
    , vertPadding : Float
    , nodeWidth : Float
    , minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    , iterations : Int}

defaults : Options
defaults = { initializeX = (\x -> toFloat x * 50 )
           , initializeY = (\y -> toFloat y * 50 )
           , edgeElasticity = identity
           , vertPadding = 20
           , nodeWidth = 25
           , minX = 0
           , maxX = 200
           , minY = 0
           , maxY = 200
           , iterations = 4 }

-----------
-- TYPES --
-----------

type alias Node =
    { label : String
    , x : Float
    , y : Float
    , throughput : Float }

type alias Edge =
    { startX : Float
    , startY : Float
    , endX : Float
    , endY : Float
    , throughput : Float }

type alias Diagram =
    { nodes : List Node
    , edges : List Edge }

type alias Inputs =
    { nodes : List (Graph.Node String)
    , flows : List (Graph.Edge Float) }

type alias NodeContext = Graph.NodeContext String Float

type alias LayoutColumn = List NodeContext

type alias Layout = List LayoutColumn

type alias ProcessorState a = State (IntDict Node) a

------------------------
-- DIAGRAM GENERATION --
------------------------

generateLayout : Inputs -> Layout
generateLayout inputs =
    Graph.fromNodesAndEdges inputs.nodes inputs.flows
        |> Graph.checkAcyclic
        |> Result.map Graph.heightLevels
        |> Result.withDefault [] -- TODO error handling

decorateNodes : List (Graph.Node String) -> IntDict Node
decorateNodes = List.map (\{id, label} -> (id, { label = label
                                               , throughput = 0
                                               , x = 0
                                               , y = 0 }))
                >> IntDict.fromList

traverseColumn : (Int -> Int -> NodeContext -> s -> s)
               -> Int
               -> LayoutColumn
               -> State s LayoutColumn
traverseColumn operation colIx column =
    column
        |> List.indexedMap (\rowIx ctx -> (colIx, rowIx, ctx))
        |> State.traverse
           (\(colIx, rowIx, ctx) -> State.modify (operation colIx rowIx ctx)
           |> State.andThen (\_ -> State.state ctx))

traverseColumns : ((Int, LayoutColumn) -> State s LayoutColumn)
                -> Layout
                -> State s Layout
traverseColumns operation layout =
    layout
        |> List.indexedMap (,)
        |> State.traverse operation

traverseLayout : (Int -> Int -> NodeContext -> s -> s)
               -> Layout
               -> State s Layout
traverseLayout operation layout =
    traverseColumns
        (\(i,column) -> traverseColumn operation i column)
        layout

fromContext : (Node -> x)
            -> x
            -> IntDict Node
            -> NodeContext
            -> x
fromContext f default nodeDict ctx =
    IntDict.get ctx.node.id nodeDict
        |> Maybe.map f
        |> withDefault default

initializeNodes : Options -> Layout -> ProcessorState Layout
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

sortColumn : LayoutColumn -> ProcessorState LayoutColumn
sortColumn column =
    State.get |> State.andThen ( \nodeDict ->
                                     column
                                         |> List.sortBy (fromContext .y 0 nodeDict)
                                         |> State.state )

weightedAverage : List (Float,Float) -> Float
weightedAverage vals =
    let
        go numerator denominator vals =
            case vals of
                [] -> if denominator /= 0
                      then numerator / denominator
                      else 0
                (weight,value)::rest -> go (numerator+weight*value) (denominator+weight) rest
    in
        go 0 0 vals

pullEdges : Options
          -> (Int, LayoutColumn)
          -> ProcessorState LayoutColumn
pullEdges opts (colIx, column) =
    let
        handleNode : Int -> Int -> NodeContext -> IntDict Node -> IntDict Node
        handleNode _ _ ctx nodeDict =
            IntDict.update ctx.node.id
                (Maybe.map (\node -> { node
                                         | y = IntDict.toList ctx.incoming
                                                   |> List.append (IntDict.toList ctx.outgoing)
                                                   |> List.map (Tuple.mapFirst (\id ->
                                                                                    IntDict.get id nodeDict
                                                                                        |> Maybe.map .y
                                                                                        |> withDefault 0)
                                                                    >> Tuple.mapSecond opts.edgeElasticity )
                                                   |> weightedAverage}))
                nodeDict
    in
        traverseColumn handleNode colIx column

resolveOverlaps : Options -> LayoutColumn -> ProcessorState LayoutColumn
resolveOverlaps opts column =
    let
        pushDown : Float -> NodeContext -> ProcessorState Float
        pushDown prevBound ctx =
            State.advance (\nodeDict ->
                               case IntDict.get ctx.node.id nodeDict of
                                   Nothing -> (prevBound,nodeDict)
                                   Just node ->
                                       let
                                           newY = max node.y prevBound
                                       in
                                           ( newY + node.throughput + opts.vertPadding
                                           , IntDict.insert ctx.node.id {node | y = newY} nodeDict ))

        pushUp : Float -> NodeContext -> ProcessorState Float
        pushUp prevBound ctx =
            State.advance (\nodeDict ->
                               case IntDict.get ctx.node.id nodeDict of
                                   Nothing -> (prevBound,nodeDict)
                                   Just node ->
                                       let
                                           newY = min node.y (prevBound - node.throughput - opts.vertPadding)
                                       in
                                           ( newY
                                           , IntDict.insert ctx.node.id {node | y = newY} nodeDict ))
    in
        State.foldlM pushDown 0 column
            |> State.andThen (\_ -> State.foldrM (flip pushUp) opts.maxY column )
            |> State.andThen (\_ -> State.state column)

relaxNodes : Options
           -> Layout
           -> ProcessorState Layout
relaxNodes opts =
    let
        relaxColumn : (Int, LayoutColumn) -> ProcessorState LayoutColumn
        relaxColumn col =
            pullEdges opts col
                |> State.andThen (\col -> sortColumn col)
                |> State.andThen (\col -> resolveOverlaps opts col)

        relaxLayout : Layout -> ProcessorState Layout
        relaxLayout layout =
            traverseColumns relaxColumn layout
                |> State.map List.reverse
                |> State.andThen (\l -> traverseColumns relaxColumn l)
                |> State.map List.reverse

        go : Int -> Layout -> ProcessorState Layout
        go i layout =
            if i >= opts.iterations
            then
                State.state layout
            else
                relaxLayout layout
                    |> State.andThen (\l -> go (i+1) l)

    in
        go 0

outputDiagram : Options -> Inputs -> Layout -> ProcessorState Diagram
outputDiagram opts inputs layout =
    State.embed (\nodeDict ->
                      { nodes = IntDict.values nodeDict
                      , edges =
                          let
                              edgeKey x y = (max x y, min x y)

                              updateEdges : Int -> Int -> NodeContext -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
                              updateEdges _ _ ctx = placeIncomingEdges ctx >> placeOutgoingEdges ctx

                              placeIncomingEdges : NodeContext -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
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

                              placeOutgoingEdges : NodeContext -> Dict (Int,Int) Edge -> Dict (Int,Int) Edge
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


generateDiagram : Options -> Inputs -> Diagram
generateDiagram opts inputs =
    generateLayout inputs
        |> initializeNodes opts
        |> State.andThen (relaxNodes opts)
        |> State.andThen (outputDiagram opts inputs)
        |> State.finalValue (decorateNodes inputs.nodes)


---------------
-- RENDERING --
---------------

render : Options -> Diagram -> Html msg
render opts {nodes, edges} =
    let
        renderNode : Node -> Svg msg
        renderNode {label, x, y, throughput} =
            Svg.rect
                [ Attr.x <| toString x
                , Attr.y <| toString y
                , Attr.width <| toString opts.nodeWidth
                , Attr.height <| toString throughput
                , Attr.fill "blue"
                , Attr.stroke "black"
                , Attr.strokeWidth "2" ]
                [ Svg.text_ [] [ Svg.text label ]]

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
            [ Attr.width "800"
            , Attr.height "800"
            , Attr.viewBox <| String.join " " [ toString opts.minX
                                              , toString opts.minY
                                              , toString <| opts.maxX - opts.minX
                                              , toString <| opts.maxY - opts.minY ]]
            [ Svg.g [] (List.map renderNode nodes)
            , Svg.g [] (List.map renderEdge edges) ]
