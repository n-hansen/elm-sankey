{- Copyright (C) 2018 Nicholas Hansen
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3 or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/ -}
module Sankey exposing (..)

import Dict exposing (Dict)
import Graph exposing (Graph, AcyclicGraph)
import IntDict exposing (IntDict)
import List
import Maybe exposing (withDefault)
import State exposing (State)
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
defaults = { initializeX = toFloat
           , initializeY = toFloat
           , edgeElasticity = identity
           , vertPadding = 1
           , nodeWidth = 30
           , minX = 0
           , maxX = 100
           , minY = 0
           , maxY = 100
           , iterations = 1 }

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

---------------
-- FUNCTIONS --
---------------

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

fromContext : (Node a -> x)
            -> x
            -> IntDict (Node a)
            -> NodeContext a
            -> x
fromContext f default nodeDict ctx =
    IntDict.get ctx.node.id nodeDict
        |> Maybe.map f
        |> withDefault default

initializeNodes : Options -> Layout a -> ProcessorState a (Layout a)
initializeNodes opts =
    traverseLayout (\colIx rowIx ctx ->
                        IntDict.update ctx.node.id
                        (Maybe.map (\node -> { node
                                                 | x = opts.initializeX rowIx
                                                 , y = opts.initializeY colIx
                                                 , throughput = ctx.incoming
                                                 |> IntDict.values
                                                 |> List.sum })))

sortColumn : LayoutColumn a -> ProcessorState a (LayoutColumn a)
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
                                                                                        |> Maybe.map .y
                                                                                        |> withDefault 0)
                                                                    >> Tuple.mapSecond opts.edgeElasticity )
                                                   |> weightedAverage}))
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
                                           ( newY + node.throughput + opts.vertPadding
                                           , IntDict.insert ctx.node.id {node | y = newY} nodeDict ))

        pushUp : Float -> NodeContext a -> ProcessorState a Float
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
                    |> State.andThen (\l -> go (i+1) l)

    in
        go 0

generateDiagram : Options -> Inputs a -> Layout a -> ProcessorState a (Diagram a)
generateDiagram opts inputs layout =
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
