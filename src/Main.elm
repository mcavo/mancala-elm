module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class,style)
import Html.Events exposing (onClick)
import Html.App

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

-- STYLE

size : Int
size = 6

boardSize : Int
boardSize = 14

value : Int
value = 4

boxHeight : Int
boxHeight = 130 

menuBoxStyle : Attribute Msg
menuBoxStyle = style 
    [ ("border-radius", "10px 10px 10px 10px")
    , ("border", "9px double #ffffff")
    , ("background-color", "#1abc9c")
    , ("color", "#ffffff")
    , ("height", "300px")
    , ("width", "100%")
    , ("padding", "40px 15px 0")
    ]

borderStyle : Attribute Msg
borderStyle = style
    [ ("border-top", "3px solid #ffffff") ]

bigBoxStyle : Attribute Msg
bigBoxStyle = style
    [ ("height", toString(2 * boxHeight) ++ "px")
    , ("line-height", "240px")
    , ("color", "#ffffff")
    , ("border", "5px solid #ffffff")
    , ("padding-left", "0px")
    , ("padding-right", "0px")
    ]

verticalStyle : Attribute Msg
verticalStyle = style
    [ ("vertical-align", "middle")
    , ("display", "inline-block")
    ]

smallBoxStyle : Attribute Msg
smallBoxStyle = style
    [ ("height", toString(boxHeight) ++ "px")
    , ("line-height", "110px")
    , ("color", "#ffffff")
    , ("border", "5px solid #ffffff")
    ]

player1Style : Attribute Msg
player1Style = style
    [ ("background-color", "#1abc9c") ]

player2Style : Attribute Msg
player2Style = style
    [ ("background-color", "#a6aaa9") ]

pxnull : Attribute Msg
pxnull = style
    [ ("padding-left", "0px")
    , ("padding-right", "0px")
    ]

-- MODEL

type Turn = Player1 | Player2

type Status = Menu | Playing | EndGame

type alias Board = 
    { list : List Int
    } 

initBoard : Board
initBoard =
    { list = List.append
                ( List.append (List.map (\n -> value) [1..size]) [0] )
                ( List.append (List.map (\n -> value) [1..size]) [0] )
    }

type alias Model =
    { status : Status
    , board : Board
    , turn : Turn
    }


init : ( Model, Cmd Msg )
init =
    ( {status=Menu, board=initBoard, turn=Player1}, Cmd.none )



-- MESSAGES


type Msg
    = NoOp | Start | Move Int

getEndGameText : Board -> String
getEndGameText board = 
    if (getSeeds size board.list) == (getSeeds (boardSize-1) board.list)
    then "Its a tie!"
    else if (getSeeds size board.list) < (getSeeds (boardSize-1) board.list)
    then "Player2 wins!"
    else "Player1 wins!"

getPlayer2Row : Model ->  List (Html Msg)
getPlayer2Row model = 
    if model.turn == Player2
    && model.status /= EndGame
    then
        List.map 
            (\(n,m) -> 
                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull, onClick( Move m )]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.map2 (,) (List.reverse(getSubList (size+1) (boardSize-1) model.board.list )) (List.reverse(List.map (\n -> n) [(size+1)..(boardSize-2)])))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.reverse(getSubList (size+1) (boardSize-1) model.board.list ))

getPlayer1Row : Model ->  List (Html Msg)
getPlayer1Row model = 
    if model.turn == Player1
    && model.status /= EndGame
    then
        List.map 
            (\(n,m) -> 
                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull, onClick( Move m )]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.map2 (,) (getSubList 0 (size) model.board.list) (List.map (\n -> n) [0..(size-1)]))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (getSubList 0 (size) model.board.list)

getMenuView : Model -> Html Msg
getMenuView model = 
    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-4 col-md-4 text-center"] 
              [ div [menuBoxStyle] 
                    [ div [class "h2"]
                          [text "Mancala"]
                    , p [borderStyle] []
                    , p [class "h3"] [text "Rules"]
                    , p [class "h3", onClick (Start)] [text "Play"] ]]]

getPlayingView : Model -> Html Msg
getPlayingView model = 
    div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [verticalStyle] [text (toString(getSeeds (boardSize-1) model.board.list)) ]]]
        , div [ class "col-md-6", pxnull]
              ((getPlayer2Row model) ++ (getPlayer1Row model))
        , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [verticalStyle] [text (toString(getSeeds size model.board.list)) ]]]
        ]

getEndGameView : Model -> Html Msg 
getEndGameView model = 
    div []
        [ div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
              [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [verticalStyle] [text (toString(getSeeds (boardSize-1) model.board.list)) ]]]
              , div [ class "col-md-6", pxnull]
                    ((getPlayer2Row model) ++ (getPlayer1Row model))
              , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [verticalStyle] [text (toString(getSeeds size model.board.list)) ]]]
              ]
        , div [ class "row text-center", style [ ("padding", "50px 15px 0") ] ]
              [ h2 [] [ text ( getEndGameText model.board ) ]]
        ] 

-- VIEW

view : Model -> Html Msg
view model = 
    case model.status of
        Menu -> 
            getMenuView model
        Playing ->
            getPlayingView model
        EndGame ->
            getEndGameView model


-- UPDATE

diffBoardSides : Int -> Int -> Bool
diffBoardSides i1 i2 =
    if ((i1<size && i2<size)
    || (i1>size && i1<(boardSize-1) && i2>size && i2<(boardSize-1)))
    then
        False
    else
        True

getOpposite : Int -> Int
getOpposite hole =
    if hole <= size
    then
        getSeeds hole (List.reverse(List.map (\n -> n) (13::[(size+1)..(boardSize-2)])))
    else
        getSeeds (hole-size-1) (List.reverse(List.map (\n -> n) (6::[0..(size-1)])))

getSeeds : Int -> List Int -> Int
getSeeds x list =
    case (x, list) of
        (_, []) ->
            -1
        (0, head::tail) ->
            head
        (x, head::tail) ->
            getSeeds (x-1) tail

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
    |> (\n -> {n | list = (addSeedsToHole n.list (getSeeds hole1 n.list) playerHole)})
    |> (\n -> {n | list = (addSeedsToHole n.list (getSeeds hole2 n.list) playerHole)})
    |> (\n -> {n | list = (removeSeedsFromHole n.list hole1)})
    |> (\n -> {n | list = (removeSeedsFromHole n.list hole2)})

checkCapture : Model -> Int -> Int -> Model
checkCapture model position seeds =
    if (((position+seeds)%boardSize) /= size)
    && (((position+seeds)%boardSize) /= (boardSize-1))
    && ((getSeeds ((position+seeds)%boardSize) model.board.list) == 1) -- This hole was empty
    && ((getSeeds (getOpposite((position+seeds)%boardSize)) model.board.list) /= 0)
    && not (diffBoardSides position ((position+seeds)%boardSize))
    then 
        { model | board=(capture model.board ((position+seeds)%boardSize) (getOpposite ((position+seeds)%14)) (getPlayerHole model.turn))}
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

updateModel : Model -> Int -> Int -> Model
updateModel model position seeds = 
    ({model | board=(updateBoard model.board position seeds)})
    |> (\n -> (checkCapture n position seeds))

updateTurn : Turn -> Int -> Int -> Turn 
updateTurn turn position seeds = 
    case turn of
        Player1 -> 
            if ((position+seeds)%boardSize) == size
            || seeds == 0
            then Player1
            else Player2
        Player2 -> 
            if ((position+seeds)%boardSize) == (boardSize-1)
            || seeds == 0
            then Player2
            else Player1

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

checkNoMoves : Turn -> List Int -> List Int
checkNoMoves turn list =
    case turn of
        Player1 -> 
            if checkEmptyList 0 size list
            then 
                (addSeedsToHole list (List.sum (getSubList (size+1) (boardSize-1) list)) (boardSize-1) )
                |> (\n -> clearBoard n (size+1) (boardSize-1))
            else
                list
        Player2 -> 
            if checkEmptyList (size+1) (boardSize-1) list
            then 
                (addSeedsToHole list (List.sum (getSubList 0 size list)) size )
                |> (\n -> clearBoard n 0 size)
            else
                list

checkNoMovesBoard : Turn -> Board -> Board
checkNoMovesBoard turn board =
    {board | list = checkNoMoves turn board.list}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( {model | status=Menu}, Cmd.none )
        Start ->
            ( {model | status=Playing}, Cmd.none )
        Move position ->
            ( (updateModel model position (getSeeds position model.board.list)), Cmd.none )
            |> (\(n,m) -> ({n | turn=(updateTurn n.turn position (getSeeds position model.board.list))}, m))
            |> (\(n,m) -> ({n | board=(checkNoMovesBoard n.turn n.board)}, m))
            |> (\(n,m) -> ({n | status=(updateStatus n.board)}, m))


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MAIN


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

