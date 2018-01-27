{- Copyright (C) 2018 Nicholas Hansen
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3 or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/ -}
module Sankey exposing (..)

import Graph exposing (Graph, AcyclicGraph)
import IntDict exposing (IntDict)
import List
import Maybe exposing (withDefault)
import State exposing (State)
import Tuple

type alias Options =
    { initializeX : Int -> Float
    , initializeY : Int -> Float
    , edgeElasticity : Float -> Float }

defaults : Options
defaults = { initializeX = toFloat
           , initializeY = toFloat
           , edgeElasticity = identity }

type alias Node a =
    { label : a
    , x : Float
    , y : Float
    , throughput : Float }

type alias Edge =
    { label : String
    , startX : Float
    , startX : Float
    , endX : Float
    , endY : Float
    , throughput : Float }

type alias Diagram a =
    { nodes : List (Node a)
    , edges : List Edge }

type alias Inputs a =
    { nodes : List (Graph.Node a)
    , flows : List (Graph.Edge Float) }

decorateNodes : List (Graph.Node a) -> IntDict (Node a)
decorateNodes = List.map (\{id, label} -> (id, { label = label
                                               , throughput = 0
                                               , x = 0
                                               , y = 0 }))
                >> IntDict.fromList

type alias NodeContext a = Graph.NodeContext a Float

type alias LayoutColumn a = List (NodeContext a)

type alias Layout a = List (LayoutColumn a)

type alias ProcessorState a b = State (IntDict (Node a)) b

traverseColumn : (Int -> Int -> NodeContext a -> IntDict (Node a) -> IntDict (Node a))
               -> Int
               -> LayoutColumn a
               -> ProcessorState a (LayoutColumn a)
traverseColumn operation col column =
    column
        |> List.indexedMap (\row ctx -> (col, row, ctx))
        |> State.traverse
           (\(col, row, ctx) -> State.modify (operation col row ctx)
           |> State.andThen (\_ -> State.state ctx))

traverseColumns : ((Int, LayoutColumn a) -> ProcessorState a (LayoutColumn a))
                -> Layout a
                -> ProcessorState a (Layout a)
traverseColumns operation layout =
    layout
        |> List.indexedMap (,)
        |> State.traverse operation


sortColumn : LayoutColumn a -> ProcessorState a (LayoutColumn a)
sortColumn column =
    State.get |> State.andThen ( \nodeDict ->
                                     column
                                         |> List.sortBy (\ctx ->
                                                             IntDict.get (ctx.node.id) nodeDict
                                                                 |> Maybe.map .y
                                                                 |> withDefault 0 )
                                         |> State.state )

traverseLayout : (Int -> Int -> NodeContext a -> IntDict (Node a) -> IntDict (Node a))
               -> Layout a
               -> ProcessorState a (Layout a)
traverseLayout operation layout =
    traverseColumns
        (\(i,column) ->
             column
                 |> sortColumn
                 |> State.andThen (traverseColumn operation i))
        layout

initializeNodes : Options -> Layout a -> ProcessorState a (Layout a)
initializeNodes opts =
    traverseLayout (\col row ctx ->
                        IntDict.update ctx.node.id
                        (Maybe.map (\node -> { node
                                                 | x = opts.initializeX row
                                                 , y = opts.initializeY col
                                                 , throughput = ctx.incoming
                                                 |> IntDict.values
                                                 |> List.sum })))

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

pullEdges : Options -> Int -> Int -> NodeContext a -> IntDict (Node a) -> IntDict (Node a)
pullEdges opts _ _ ctx nodeDict =
    IntDict.update ctx.node.id
        (Maybe.map (\node -> { node
                                 | y = IntDict.toList ctx.incoming
                                 |> List.append (IntDict.toList ctx.outgoing)
                                 |> List.map
                                   ( Tuple.mapFirst (\id -> IntDict.get id nodeDict |> Maybe.map .y |> withDefault 0)
                                   >> Tuple.mapSecond opts.edgeElasticity )
                                 |> weightedAverage}))
        nodeDict
