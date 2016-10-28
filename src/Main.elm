module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class,style, src)
import Html.Events exposing (onClick)
import Html.App
import MancalaAPI exposing (..)

-- TODO: undo
-- TODO: ai



-- STYLE

boxHeight : Int
boxHeight = 130

player1Color : String
player1Color = "#1abc9c"

player2Color : String
player2Color = "#a6aaa9"

getPlayerColor : MancalaAPI.Turn -> String
getPlayerColor turn =
    case turn of
        Player1 -> "#1abc9c"
        Player2 -> "#a6aaa9"
--      NoTurn -> "#252727"

menuBoxStyle : Attribute Msg
menuBoxStyle = style 
    [ ("border-radius", "10px 10px 10px 10px")
    , ("border", "9px double #ffffff")
    , ("background-color", player1Color)
    , ("color", "#ffffff")
    , ("height", "300px")
    , ("width", "100%")
    , ("padding", "40px 15px 0")
    ]

replayStyle : Attribute Msg
replayStyle = style 
    [ ("height", "5%")
    , ("width", "5%")
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
    [ ("background-color", player1Color) ]

player2Style : Attribute Msg
player2Style = style
    [ ("background-color", player2Color) ]

pxnull : Attribute Msg
pxnull = style
    [ ("padding-left", "0px")
    , ("padding-right", "0px")
    ]

-- MODEL

init : ( MancalaAPI.Model, Cmd Msg )
init =
    ( {status=Menu, board=MancalaAPI.initBoard, turn=Player1, feedback=[MancalaAPI.initFeedback]}, Cmd.none )



-- MESSAGES

getWinnerH2 : MancalaAPI.Board -> Html Msg
getWinnerH2 board =
    if (MancalaAPI.getElementList size board.list) == (MancalaAPI.getElementList (boardSize-1) board.list)
    then
        h2 [] [ text "Its a tie!" ]
    else if (MancalaAPI.getElementList size board.list) < (MancalaAPI.getElementList (boardSize-1) board.list)
    then 
        h2 [ style [ ("color", player2Color) ]] [ text "Player2 wins!" ]
    else 
        h2 [ style [ ("color", player1Color) ]] [ text "Player1 wins!" ]
    

type Msg
    = Back | Start | Move Int

getFeedbackView : List MancalaAPI.Feedback -> Html Msg
getFeedbackView list = 
    div [ class "row text-center", style [ ("padding", "50px 15px 0") ]]
        (List.map
            (\n ->
                h2 [ style [ ("color", getPlayerColor n.turn) ]] [ text n.msg ])
            list)

getWinnerView : MancalaAPI.Board -> Html Msg
getWinnerView board =
    div [ class "row text-center", style [ ("padding", "10px 15px 0") ]]
        [ getWinnerH2 board ]


getPlayer2Row : MancalaAPI.Model ->  List (Html Msg)
getPlayer2Row model = 
    if model.turn == Player2
    && model.status /= EndGame
    then
        List.map 
            (\(n,m) -> 
                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull, onClick( Move m )]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.map2 (,) (List.reverse(MancalaAPI.getSubList (size+1) (boardSize-1) model.board.list )) (List.reverse(List.map (\n -> n) [(size+1)..(boardSize-2)])))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.reverse(MancalaAPI.getSubList (size+1) (boardSize-1) model.board.list ))

getPlayer1Row : MancalaAPI.Model ->  List (Html Msg)
getPlayer1Row model = 
    if model.turn == Player1
    && model.status /= EndGame
    then
        List.map 
            (\(n,m) -> 
                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull, onClick( Move m )]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.map2 (,) (MancalaAPI.getSubList 0 (size) model.board.list) (List.map (\n -> n) [0..(size-1)]))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull]
                    [ h2 [verticalStyle] [ text (toString(n)) ] ]
            ) 
            (MancalaAPI.getSubList 0 (size) model.board.list)

getMenuView : MancalaAPI.Model -> Html Msg
getMenuView model = 
    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-4 col-md-4 text-center"] 
              [ div [menuBoxStyle] 
                    [ div [class "h2"]
                          [text "Mancala"]
                    , p [borderStyle] []
                    , p [class "h3"] [text "Rules"]
                    , p [class "h3", onClick (Start)] [text "Play"] ]]]

getPlayingView : MancalaAPI.Model -> Html Msg
getPlayingView model = 
    div []
        [ div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
              [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [verticalStyle] [text (toString(MancalaAPI.getElementList (boardSize-1) model.board.list)) ]]]
              , div [ class "col-md-6", pxnull]
                    ((getPlayer2Row model) ++ (getPlayer1Row model))
              , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [verticalStyle] [text (toString(MancalaAPI.getElementList size model.board.list)) ]]]
              ]
        , getFeedbackView model.feedback
        ] 

getEndGameView : MancalaAPI.Model -> Html Msg 
getEndGameView model = 
    div []
        [ div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
              [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [verticalStyle] [text (toString(MancalaAPI.getElementList (boardSize-1) model.board.list)) ]]]
              , div [ class "col-md-6", pxnull]
                    ((getPlayer2Row model) ++ (getPlayer1Row model))
              , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [verticalStyle] [text (toString(MancalaAPI.getElementList size model.board.list)) ]]]
              ]
        , getFeedbackView model.feedback
        , getWinnerView model.board
        , div [ class "row text-center", style [ ("padding", "20px 15px 0") ] ]
              [ img [src "images/replay.png", replayStyle, onClick( Back )] []
              ]
        ] 

-- VIEW

view : MancalaAPI.Model -> Html Msg
view model = 
    case model.status of
        Menu -> 
            getMenuView model
        Playing ->
            getPlayingView model
        EndGame ->
            getEndGameView model


-- UPDATE

update : Msg -> MancalaAPI.Model -> ( MancalaAPI.Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            ( {model | status=Menu}, Cmd.none )
            |> (\(n,m) -> ({n | feedback=[]}, m))
        Start ->
            ( {model | status=Playing}, Cmd.none )
            |> (\(n,m) -> ({n | board=MancalaAPI.initBoard}, m))
            |> (\(n,m) -> ({n | feedback=[MancalaAPI.initFeedback]}, m))
            |> (\(n,m) -> ({n | turn=Player1}, m))
        Move position ->
            ( (updateMoveModel model position (MancalaAPI.getElementList position model.board.list)), Cmd.none )
            |> (\(n,m) -> ((MancalaAPI.updateTurnModel n position (MancalaAPI.getElementList position model.board.list)), m))
            |> (\(n,m) -> ((MancalaAPI.checkNoMovesModel n), m))
            |> (\(n,m) -> ((MancalaAPI.updateStatusModel n), m))


-- SUBSCRIPTIONS


subscriptions : MancalaAPI.Model -> Sub Msg
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

-- TODO: Undo

