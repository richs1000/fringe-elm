module Search exposing (..)

-- import Graph exposing (..)


type alias NodeId =
    String


type alias EdgeWeight =
    Int


type alias HeuristicValue =
    Int


type SearchAlgorithm
    = BreadthFirstSearch
    | DepthFirstSearch
    | UniformCostSearch
    | GreedySearch
    | AStarSearch


type alias FringeElement =
    ( NodeId, EdgeWeight, HeuristicValue )


byName : FringeElement -> NodeId
byName ( n, _, _ ) =
    n


byCost : FringeElement -> EdgeWeight
byCost ( _, g, _ ) =
    g


byHeuristic : FringeElement -> HeuristicValue
byHeuristic ( _, _, h ) =
    h


byCostAndHeuristic : FringeElement -> HeuristicValue
byCostAndHeuristic ( _, g, h ) =
    g + h


getFirstFringeItem : List FringeElement -> FringeElement
getFirstFringeItem fringe =
    Maybe.withDefault ( "Z", 0, 0 ) (List.head fringe)


getLastFringeItem : List FringeElement -> FringeElement
getLastFringeItem fringe =
    Maybe.withDefault ( "Z", 0, 0 ) (List.head (List.reverse fringe))


getLowestCostFringeItem : List FringeElement -> FringeElement
getLowestCostFringeItem fringe =
    fringe
        |> List.sortBy byName
        |> List.sortBy byCost
        |> List.head
        |> Maybe.withDefault ( "Z", 0, 0 )


getLowestHeuristicFringeItem : List FringeElement -> FringeElement
getLowestHeuristicFringeItem fringe =
    fringe
        |> List.sortBy byName
        |> List.sortBy byHeuristic
        |> List.head
        |> Maybe.withDefault ( "Z", 0, 0 )


getLowestAStarFringeItem : List FringeElement -> FringeElement
getLowestAStarFringeItem fringe =
    fringe
        |> List.sortBy byName
        |> List.sortBy byCostAndHeuristic
        |> List.head
        |> Maybe.withDefault ( "Z", 0, 0 )


convertToSearchAlgorithm : Int -> SearchAlgorithm
convertToSearchAlgorithm index =
    case index of
        1 ->
            BreadthFirstSearch

        2 ->
            DepthFirstSearch

        3 ->
            UniformCostSearch

        4 ->
            GreedySearch

        _ ->
            AStarSearch


searchAlgorithmString : SearchAlgorithm -> String
searchAlgorithmString index =
    case index of
        BreadthFirstSearch ->
            "Breadth-First Search"

        DepthFirstSearch ->
            "Depth-First Search"

        UniformCostSearch ->
            "Uniform Cost Search"

        GreedySearch ->
            "Greedy Search"

        AStarSearch ->
            "A* Search"



-- breadthFirstSearch : Graph -> NodeId -> NodeId -> Maybe (List NodeId)
-- breadthFirstSearch graph startNode endNode =
--     genericSearch graph startNode endNode
--
--
-- genericSearch : Graph -> NodeId -> NodeId -> Maybe (List NodeId)
-- genericSearch graph startNode endNode =
--     let
--         searchHelper openList closedList searchTree =
--             case openList of
--                 -- if the open list is empty then we're done
--                 [] ->
--                     Nothing
--
--                 -- else pull the next item off the open list
--                 firstNode :: restOfList ->
--                     --  if the next item is what we're looking for then
--                     --  return the path
--                     if (firstNode == endNode) then
--                         Just (List.reverse (endNode :: (unwindSearchTree searchTree endNode)))
--                     else
--                         let
--                             neighbors =
--                                 List.filter (edgeExists graph firstNode) graph.nodes
--                                     |> List.filter (\n -> not (visited openList closedList n))
--
--                             -- DFS put at start
--                             -- BFS put at end
--                             openList' =
--                                 (List.append restOfList neighbors)
--
--                             searchTree' =
--                                 (( firstNode, neighbors ) :: searchTree)
--
--                             -- start again...
--                         in
--                             case neighbors of
--                                 [] ->
--                                     (searchHelper openList' (firstNode :: closedList) searchTree')
--
--                                 _ ->
--                                     (searchHelper openList' (firstNode :: closedList) searchTree')
--     in
--         searchHelper [ startNode ] [] []
--
--
-- unwindSearchTree : List ( NodeId, List NodeId ) -> NodeId -> List NodeId
-- unwindSearchTree searchTree lastNode =
--     case searchTree of
--         [] ->
--             []
--
--         ( node, children ) :: rest ->
--             if (List.member lastNode children) then
--                 node :: (unwindSearchTree rest node)
--             else
--                 unwindSearchTree rest lastNode
--
--
-- pathExists : Graph -> NodeId -> NodeId -> Bool
-- pathExists graph startNode endNode =
--     let
--         s =
--             breadthFirstSearch graph startNode endNode
--     in
--         case s of
--             Nothing ->
--                 False
--
--             Just _ ->
--                 True
