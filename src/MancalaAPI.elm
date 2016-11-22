module MancalaAPI exposing (..)

size : Int
size = 6

boardSize : Int
boardSize = 14

minimaxDepth : Int
minimaxDepth = 6

aiplayer : Turn
aiplayer = Player2


value : Int
value = 4

type Turn = Player1 | Player2 -- Player2 is considered to be the AI

type Status = Menu | Playing | EndGame

type alias Feedback =
    { turn : Turn
    , msg : String
    }

type alias Board = 
    { list : List Int
    }

type alias Model =
    { status : Status
    , board : Board
    , turn : Turn
    , feedback : List Feedback
    }

type alias MinimaxNode =
    { model : Model
    , previusMovement : Int
    , depth : Int
    }

type alias MinimaxDecision =
    {
      position : Int
    , score : Int 
    }


-- AI

--A partir de un nodo, conseguir las posibles jugadas

modelToNode : Model -> MinimaxNode
modelToNode nodeModel = { model = nodeModel, previusMovement = -1, depth = 0  }

posiblePlays : MinimaxNode -> List Int
posiblePlays node = 
    case node.model.turn of
        Player1 ->
            ([0..size-1]) 
            |> (List.filter (isNotEmpty node.model.board.list))
        Player2 ->
            ([(size+1)..boardSize-1])
            |> (List.filter (isNotEmpty node.model.board.list))

isNotEmpty : List Int -> Int -> Bool
isNotEmpty list n = 
    case (getElementList n list) of
        0 ->
            False
        _ -> 
            True


makePlay : MinimaxNode -> Int -> MinimaxNode
makePlay node play =
    {
        model = 
            ( (updateMoveModel node.model play (getElementList play node.model.board.list)) )
            |> (\(n) -> ((updateTurnModel n play (getElementList play node.model.board.list))))
            |> (\(n) -> ((checkNoMovesModel n)))
            |> (\(n) -> ((updateStatusModel n)))
    ,   previusMovement = play
    ,   depth = node.depth+1
    }

getHeuristicFromNode : MinimaxNode -> Int
getHeuristicFromNode node = getHeuristic node.model


getHeuristic : Model -> Int
getHeuristic model = 
    if (model.status == EndGame)
    then
        if((player1Score model)== (player2Score model))
        then
            0
        else
            if(((player2Score model) - (player1Score model) ) < 0)
            then
                ((player2Score model)- (player1Score model))-300
            else
                ((player2Score model) - (player1Score model) )+300
    else
        ((player2Score model)- (player1Score model) )



player1Score : Model ->Int
player1Score model = getElementList size model.board.list

player2Score : Model ->Int
player2Score model = getElementList (boardSize-1) model.board.list




getBestMove : Model -> Int
getBestMove model = (getBestMoveFromNode (modelToNode model)).position

getBestMoveFromNode : MinimaxNode ->MinimaxDecision
getBestMoveFromNode node =
    if(node.depth == minimaxDepth)
        then
            {position = node.previusMovement ,score = (getHeuristicFromNode node) }
        else
            if (node.model.status == EndGame)
                then
                    {position = node.previusMovement ,score = (getHeuristicFromNode node) }
                else
                    if(node.model.turn == aiplayer)
                        then
                            if (node.depth == 0)
                                then
                                    List.foldr maxNode {position = -1, score = -1000} (List.map getBestMoveFromNode (List.map (makePlay node) (posiblePlays node)))
                                else
                                    {position = node.previusMovement , score = (List.foldr maxNode {position = -1, score = -1000} (List.map getBestMoveFromNode (List.map (makePlay node) (posiblePlays node)))).score}
                        else
                            if (node.depth == 0)
                                then
                                    List.foldr minNode {position = -1, score = 1000} (List.map getBestMoveFromNode (List.map (makePlay node) (posiblePlays node)))
                                else
                                    {position = node.previusMovement , score = (List.foldr minNode {position = -1, score = 1000} (List.map getBestMoveFromNode (List.map (makePlay node) (posiblePlays node)))).score}




minNode : MinimaxDecision -> MinimaxDecision -> MinimaxDecision
minNode decisionA decisionB = 
    if (decisionA.score > decisionB.score)
        then 
            decisionB
        else
            decisionA

maxNode : MinimaxDecision -> MinimaxDecision -> MinimaxDecision
maxNode decisionA decisionB = 
    if (decisionA.score > decisionB.score)
        then 
            decisionA
        else
            decisionB


-- END AI



turnToString : Turn -> String
turnToString turn =
    case turn of
        Player1 -> "Player1"
        Player2 -> "Player2"

initBoard : Board
initBoard =
    { list = List.append
                ( List.append (List.map (\n -> value) [1..size]) [0] )
                ( List.append (List.map (\n -> value) [1..size]) [0] )
    }

initFeedback : Feedback
initFeedback = 
    { turn = Player1
    , msg = "Starts Player1" }

turnFeedback : Turn -> Feedback
turnFeedback t =
    { turn = t
    , msg = (turnToString t) ++ " turn" }

extraTurnFeedback : Turn -> Feedback
extraTurnFeedback t =
    { turn = t
    , msg = "Extra turn!" }

captureFeedback : Turn -> Feedback
captureFeedback t =
    { turn = t
    , msg = "Capture!" }

noMovesFeedback : Turn -> Feedback
noMovesFeedback t =
    { turn = t
    , msg = "No more moves!" }    


-- API
-- Returns a sublist from io inclusive to ie exclusive
getSubList : Int -> Int -> List a -> List a
getSubList io ie list = 
    case (io, ie, list) of
        (_, _, []) ->
            []
        (_, 0, list) ->
            []
        (0, y, head::sublist) ->
            head::getSubList 0 (y-1) sublist
        (x, y, head::sublist) ->
            getSubList (x-1) (y-1) sublist

getElementList : Int -> List Int -> Int
getElementList x list =
    case (x, list) of
        (_, []) ->
            -1
        (0, head::tail) ->
            head
        (x, head::tail) ->
            getElementList (x-1) tail

diffBoardSides : Int -> Int -> Bool
diffBoardSides i1 i2 =
    if ((i1<size && i2<size)
    || (i1>size && i1<(boardSize-1) && i2>size && i2<(boardSize-1)))
    then
        False
    else
        True

getOpposite : Int -> Int -- Rehacer
getOpposite hole =
    if hole <= size
    then
        getElementList hole (List.reverse(List.map (\n -> n) (13::[(size+1)..(boardSize-2)])))
    else
        getElementList (hole-size-1) (List.reverse(List.map (\n -> n) (6::[0..(size-1)])))

getPlayerHole : Turn -> Int
getPlayerHole turn =
    case turn of
        Player1 -> size
        Player2 -> boardSize-1

addOneMoreSeed : Int -> Int -> Int
addOneMoreSeed position seeds =
    if (position < (seeds % (2*(size+1))))
    then 1
    else 0

checkEmptyList : Int -> Int -> List Int -> Bool
checkEmptyList io ie list =
    case (io, ie, list) of
        (_, _, []) -> True
        (_, 0, head::tail) -> True
        (0, y, head::tail) -> if head /= 0 then False else checkEmptyList 0 (y-1) tail
        (x, y, head::tail) -> checkEmptyList (x-1) (y-1) tail

addSeedsToHole : List Int -> Int -> Int -> List Int
addSeedsToHole list seeds hole =
    case (list, seeds, hole) of
        ([],_,_) -> []
        (head::tail, seeds, 0) -> (head+seeds)::tail
        (head::tail, seeds, x) -> head::(addSeedsToHole tail seeds (x-1))

removeSeedsFromHole : List Int -> Int -> List Int
removeSeedsFromHole list index =
    case (list, index) of
        ([], _) -> []
        (head::tail, 0) -> 0::tail
        (head::tail, x) -> head::(removeSeedsFromHole tail (x-1))

capture : Board -> Int -> Int -> Int -> Board
capture board hole1 hole2 playerHole =
    (board)
    |> (\n -> {n | list = (addSeedsToHole n.list (getElementList hole1 n.list) playerHole)})
    |> (\n -> {n | list = (addSeedsToHole n.list (getElementList hole2 n.list) playerHole)})
    |> (\n -> {n | list = (removeSeedsFromHole n.list hole1)})
    |> (\n -> {n | list = (removeSeedsFromHole n.list hole2)})

checkCapture : Model -> Int -> Int -> Model
checkCapture model position seeds = -- Validate posible positions
    if (((position+seeds)%boardSize) /= size)
    && (((position+seeds)%boardSize) /= (boardSize-1))
    && ((getElementList ((position+seeds)%boardSize) model.board.list) == 1) -- This hole was empty
    && ((getElementList (getOpposite((position+seeds)%boardSize)) model.board.list) /= 0)
    && not (diffBoardSides position ((position+seeds)%boardSize))
    then 
        ({ model | board=(capture model.board ((position+seeds)%boardSize) (getOpposite ((position+seeds)%14)) (getPlayerHole model.turn))})
        |> (\n -> ({ n | feedback =  (List.append n.feedback [captureFeedback n.turn])}))
    else
        model

updateList : Board -> Int -> Int -> List Int
updateList board position seeds =
    List.map
        (\(n,m) -> 
                if m/= position
                then
                    (n + (seeds // boardSize) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds))
                else
                    ((seeds // boardSize) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds)))
        (List.map2 (,) board.list (List.map (\n -> n) [0..(boardSize-1)]))


updateBoard : Board -> Int -> Int -> Board
updateBoard board position seeds =
    { list =
        updateList board position seeds
    }

updateMoveModel : Model -> Int -> Int -> Model
updateMoveModel model position seeds = 
    ({model | board=(updateBoard model.board position seeds)})
    |> (\n -> { n | feedback =  [] })
    |> (\n -> (checkCapture n position seeds))

updateTurnModel : Model -> Int -> Int -> Model 
updateTurnModel model position seeds = 
    if seeds == 0
    then
        model
    else
        case model.turn of
            Player1 -> 
                if ((position+seeds)%boardSize) == size
                then { model | feedback = List.append model.feedback [extraTurnFeedback model.turn] }
                else { model | turn = Player2  }
            Player2 -> 
                if ((position+seeds)%boardSize) == (boardSize-1)
                then { model | feedback = List.append model.feedback [extraTurnFeedback model.turn] }
                else { model | turn = Player1  }

updateStatus : Board -> Status
updateStatus board =
    if (checkEmptyList 0 size board.list)
    && (checkEmptyList (size+1) (boardSize-1) board.list)
    then
        EndGame
    else
        Playing

clearBoard : List Int -> Int -> Int -> List Int
clearBoard list io ie =
    case (list, io, ie) of
        ([], _, _) -> []
        (list, _, 0) -> list
        (head::tail, 0, y) -> (0 :: clearBoard tail 0 (y-1))
        (head::tail, x, y) -> (head :: clearBoard tail (x-1) (y-1))

noMovesList : Turn -> List Int -> List Int
noMovesList turn list =
    case turn of
        Player1 -> 
            (addSeedsToHole list (List.sum (getSubList (size+1) (boardSize-1) list)) (boardSize-1) )
            |> (\n -> clearBoard n (size+1) (boardSize-1))
        Player2 -> 
            (addSeedsToHole list (List.sum (getSubList 0 size list)) size )
            |> (\n -> clearBoard n 0 size)
            
noMovesBoard : Turn -> Board -> Board
noMovesBoard turn board =
    {board | list = noMovesList turn board.list}

checkNoMovesModel : Model -> Model
checkNoMovesModel model =
    case model.turn of
        Player1 ->
            if checkEmptyList 0 size model.board.list
            then 
                ( { model | board = (noMovesBoard model.turn model.board)})
                |> (\n -> ({ n | feedback =  (List.append n.feedback [noMovesFeedback n.turn])}))
            else
                model
        Player2 ->
            if checkEmptyList (size+1) (boardSize-1) model.board.list
            then 
                ( { model | board = (noMovesBoard model.turn model.board)})
                |> (\n -> ({ n | feedback =  (List.append n.feedback [noMovesFeedback n.turn])}))
            else
                model

-- UPDATE

updateStatusModel : Model -> Model
updateStatusModel model =
    if (checkEmptyList 0 size model.board.list)
    && (checkEmptyList (size+1) (boardSize-1) model.board.list)
    then
        ( { model | status = EndGame})
    else
        ( { model | status = Playing})
        |> (\n -> ({ n | feedback = (List.append n.feedback [turnFeedback n.turn]) }))