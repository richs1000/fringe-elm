module Question exposing (..)

import RandomStuff exposing (..)
import Search exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> SearchAlgorithm -> Question
newQuestion randomValues searchAlgorithm =
    let
        nodeNames =
            pickABunch randomValues 5 [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J" ] "A"
                |> compressList

        fringeSize =
            List.length nodeNames

        pathWeights =
            pickABunch (List.drop fringeSize randomValues) fringeSize [1..9] 1

        nodeHeuristicValues =
            pickABunch (List.drop (2 * fringeSize) randomValues) fringeSize [1..9] 1

        fringe =
            List.map3 (\n w h -> ( n, w, h ))
                nodeNames
                pathWeights
                nodeHeuristicValues

        question' =
            [ "Given the following fringe, which fringe item will be selected next by " ++ (searchAlgorithmString searchAlgorithm)
            , ""
            , toString fringe
            , ""
            , "Each fringe item is a tuple consisting of (node name, total path cost, heuristic value)."
            , "New items are added to the right (end) of the fringe. "
            , "Ties are broken by alphabetical order."
            ]

        -- bfsAnswer =
        --     Debug.log "bfs "
        --         ( toString (getFirstFringeItem fringe)
        --         , "Breadth First Search returns the item at the front of the fringe (i.e., it treats the fringe like a queue)."
        --         )
        --
        -- dfsAnswer =
        --     Debug.log "dfs "
        --         ( toString (getLastFringeItem fringe)
        --         , "Depth First Search returns the item at the end of the fringe (i.e., it treats the fringe like a stack)."
        --         )
        --
        -- ucsAnswer =
        --     Debug.log "ucs "
        --         ( toString (getLowestCostFringeItem fringe)
        --         , "Uniform Cost Search returns the item with the lowest total path cost."
        --         )
        --
        -- greedyAnswer =
        --     Debug.log "greedy "
        --         ( toString (getLowestHeuristicFringeItem fringe)
        --         , "Greedy Search returns the item with the lowest total path cost."
        --         )
        --
        -- aStarAnswer =
        --     Debug.log "a* "
        --         ( toString (getLowestAStarFringeItem fringe)
        --         , "A* Search returns the item with the lowest combined total path cost and heuristic value."
        --         )
        answer' =
            case searchAlgorithm of
                BreadthFirstSearch ->
                    ( toString (getFirstFringeItem fringe)
                    , "Correct. Breadth First Search returns the item at the front of the fringe (i.e., it treats the fringe like a queue)."
                    )

                DepthFirstSearch ->
                    ( toString (getLastFringeItem fringe)
                    , "Correct. Depth First Search returns the item at the end of the fringe (i.e., it treats the fringe like a stack)."
                    )

                UniformCostSearch ->
                    ( toString (getLowestCostFringeItem fringe)
                    , "Correct. Uniform Cost Search returns the item with the lowest total path cost."
                    )

                GreedySearch ->
                    ( toString (getLowestHeuristicFringeItem fringe)
                    , "Correct. Greedy Search returns the item with the lowest heuristic value."
                    )

                AStarSearch ->
                    ( toString (getLowestAStarFringeItem fringe)
                    , "Correct. A* Search returns the item with the lowest combined total path cost and heuristic value."
                    )

        feedbackString =
            case searchAlgorithm of
                BreadthFirstSearch ->
                    "Incorrect. Breadth First Search returns the item at the front of the fringe (i.e., it treats the fringe like a queue)."

                DepthFirstSearch ->
                    "Incorrect. Depth First Search returns the item at the end of the fringe (i.e., it treats the fringe like a stack)."

                UniformCostSearch ->
                    "Incorrect. Uniform Cost Search returns the item with the lowest total path cost."

                GreedySearch ->
                    "Incorrect. Greedy Search returns the item with the lowest heuristic value."

                AStarSearch ->
                    "Incorrect. A* Search returns the item with the lowest combined total path cost and heuristic value."

        distractors' =
            List.map (\fe -> ( toString fe, feedbackString )) fringe

        ( _, distractors'' ) =
            List.partition (\( a, _ ) -> a == fst answer') distractors'

        format' =
            MultipleChoice
    in
        { question = question'
        , distractors = distractors''
        , answer = answer'
        , format = format'
        }



-- breadth first search
-- if index == 1 || index == 2 then
--     let
--         myOp =
--             pickOne (List.drop 10 randomValues) [ "hd e", "tl e", "tl (tl e)", "tl (tl (tl e))" ] "hd e"
--
--         question' =
--             [ "What is the value of ans after the following ML expressions are evaluated?"
--             , "e = " ++ toString (subListOne)
--             , "ans = " ++ myOp
--             ]
--
--         distractors =
--             [ ( "hd e", toString (Maybe.withDefault 0 (List.head subListOne)) )
--             , ( "tl e", toString (List.drop 1 subListOne) )
--             , ( "tl (tl e)", toString (List.drop 2 subListOne) )
--             , ( "tl (tl (tl e))", toString (List.drop 3 subListOne) )
--             ]
--
--         ( answers, distractors' ) =
--             List.partition (\( op, _ ) -> op == myOp) distractors
--
--         answer' =
--             Maybe.withDefault ( "uh oh", "uh oh" ) (List.head answers)
--
--         format' =
--             if index == 1 then
--                 MultipleChoice
--             else
--                 FillInTheBlank
--     in
--         { question = question'
--         , distractors = List.map (\( _, dis ) -> ( dis, "Incorrect." )) distractors'
--         , answer = ( snd answer', "Correct" )
--         , format = format'
--         }
--     -- list is one deep, append
-- else
--     let
--         newItem =
--             pickOne (List.drop 10 randomValues) [0..9] 0
--
--         question' =
--             [ "What is the value of ans after the following ML expressions are evaluated?"
--             , "e = " ++ toString (subListOne)
--             , "ans = " ++ toString newItem ++ " :: e"
--             ]
--
--         distractors' =
--             [ ( toString newItem, "Incorrect. The :: operation adds the item on the left to the front of the list on the right. " )
--             , ( toString (List.append subListOne [ newItem ]), "Incorrect. The :: operation adds the item on the left to the front of the list on the right. " )
--             , ( toString (newItem :: (List.drop 1 subListOne)), "Incorrect. The :: operation adds the item on the left to the front of the list on the right" )
--             ]
--
--         answer' =
--             ( toString (newItem :: subListOne)
--             , "Correct. The :: operation adds the item on the left to the front of the list on the right. "
--             )
--
--         format' =
--             if index == 3 then
--                 MultipleChoice
--             else
--                 FillInTheBlank
--     in
--         { question = question'
--         , distractors = distractors'
--         , answer = ( fst answer', "Correct" )
--         , format = format'
--         }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
