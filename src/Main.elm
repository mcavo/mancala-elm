module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, style, src)
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

menuBoxStyle : Attribute Msg
menuBoxStyle = style 
    [ ("border-radius", "10px 10px 10px 10px")
    , ("border", "9px double #ffffff")
    , ("background-color", player1Color)
    , ("color", "#ffffff")
    , ("height", "160%")
    , ("width", "100%")
    , ("padding", "40px 15px 0")
    ]

sectionTitle : Attribute Msg
sectionTitle = style
    [ ("font-family", "Blogger-Sans")
    , ("font-size", "40px")
    , ("color", "#16a085")
    ]

sectionSubtitle : Attribute Msg
sectionSubtitle = style
    [ ("font-family", "Blogger-Sans")
    , ("font-size", "24px")
    , ("color", "#1abc9c")
    ]

sectionText : Attribute Msg
sectionText = style
    [ ("font-family", "Blogger-Sans")
    , ("font-size", "18px")
    ]

sectionBack : Attribute Msg
sectionBack = style
    [ ("font-family", "Blogger-Sans")
    , ("font-size", "34px")
    , ("color", "#e67e22")
    ]

section : Attribute Msg
section = style
    [ ("padding", "0px 0 20px")
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
        h2 [ class "mancala-feedback" ] [ text "Its a tie!" ]
    else if (MancalaAPI.getElementList size board.list) < (MancalaAPI.getElementList (boardSize-1) board.list)
    then 
        h2 [ class "mancala-feedback", style [ ("color", player2Color) ]] [ text "Player2 wins!" ]
    else 
        h2 [ class "mancala-feedback", style [ ("color", player1Color) ]] [ text "Player1 wins!" ]
    

type Msg
    = Back | ShowRules | Start | Move Int

getFeedbackView : List MancalaAPI.Feedback -> Html Msg
getFeedbackView list = 
    div [ class "row text-center", style [ ("padding", "50px 15px 0") ]]
        (List.map
            (\n ->
                h2 [ class "mancala-feedback", style [ ("color", getPlayerColor n.turn) ]] [ text n.msg ])
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
                button [ class "col-md-2 btn btn-link mancala-hole", smallBoxStyle, player2Style, pxnull, onClick( Move m )]
                       [ p [][text (toString(n))] ]
            ) 
            (List.map2 (,) (List.reverse(MancalaAPI.getSubList (size+1) (boardSize-1) model.board.list )) (List.reverse(List.map (\n -> n) [(size+1)..(boardSize-2)])))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull]
                    [ h2 [class "mancala-feedback", verticalStyle] [ text (toString(n)) ] ]
            ) 
            (List.reverse(MancalaAPI.getSubList (size+1) (boardSize-1) model.board.list ))

getPlayer1Row : MancalaAPI.Model ->  List (Html Msg)
getPlayer1Row model = 
    if model.turn == Player1
    && model.status /= EndGame
    then
        List.map 
            (\(n,m) -> 
                button [ class "col-md-2 btn btn-link mancala-hole", smallBoxStyle, player1Style, pxnull, onClick( Move m )]
                       [ p [][text (toString(n))] ]
            ) 
            (List.map2 (,) (MancalaAPI.getSubList 0 (size) model.board.list) (List.map (\n -> n) [0..(size-1)]))
    else
        List.map 
            (\n -> 
                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull]
                    [ h2 [class "mancala-feedback", verticalStyle] [ text (toString(n)) ] ]
            ) 
            (MancalaAPI.getSubList 0 (size) model.board.list)

getMenuView : MancalaAPI.Model -> Html Msg
getMenuView model = 
    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-4 col-md-4 text-center"] 
              [ div [menuBoxStyle] 
                    [ div [class "h2 mancala-title"]
                          [text "Mancala"]
                    , p [borderStyle] []
                    , p [ id "rules" ][ button [class "btn btn-link mancala-action", onClick (ShowRules)] [text "Rules"]]
                    , p [ id "p1v1" ][ button [class "btn btn-link mancala-action", onClick (Start)] [text "Play 1v1"] ]
                    , p [ id "pvc" ][ button [id "pvc", class "btn btn-link mancala-action"] [text "Play vs Computer"]]
                    , div [ id "diff", style [ ("display", "none") ]]
                          [ p [][ button [class "btn btn-link mancala-action", style [ ("font-size", "26px") ]] [text "Easy"]]
                          , p [][ button [class "btn btn-link mancala-action", style [ ("font-size", "26px") ]] [text "Moderate"]]
                          , p [][ button [class "btn btn-link mancala-action", style [ ("font-size", "26px") ]] [text "Hard"]]
                          , p [ id "back_btn" ][ button [class "btn btn-warning mancala-action", style [ ("font-size", "24px") ]] [text "Back"]]
                    ]]]]

getRulesView : MancalaAPI.Model -> Html Msg
getRulesView model =
    div [ class "row", style [ ("padding", "100px 15px 0") ] ]
        [ div [ class "col-md-offset-3 col-md-6"]
              [ p [ sectionTitle ] [ text "Rules" ]
              , div [ section ]
                    [ p [ sectionText ] [ text "Mancala is a game given to a family of games with similar playing board. This particular variant is sometimes called Kalah." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Object" ]
                    , p [ sectionText ] [ text "The object of the game is to capture more seeds than your opponent. Captured seeds are kept in the larger pits on the ends, whic are known as stores. Your store is in your right, your opponent's is on your left. When the game begins they are empty." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Layout" ]
                    , p [ sectionText ] [ text "The smaller pits on each side of the boards are known as houses. the bottom houses are yours, and those on the other side are your opponents. The houses all begin with an equal number of seeds in them." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Movement" ]
                    , p [ sectionText ] [ text "Players alternate turns until one player exhausts all the seeds from all the houses on this side of the board. On his turn a player selects a houseon his side of the board, takes all the seeds, and sows the seeds one-by-one into the subsequent houses in a counter-clockwise direction around the board. If he reacheshis store, then the player will place one seed in his store, gaining a point. If a player reaches the other player's store while seeding, then it is skipped." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Extra turn" ]
                    , p [ sectionText ] [ text "If a player ends his seeding by placing the last seed into his store, he gets to take another turn immediately." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Capturing" ]
                    , p [ sectionText ] [ text "When a player ends the seeding process by placing a seed into and empty house on his side of the board and there are seeds in the house directly across the board from him, he captures the last seed just sown plus all the seeds in the opponent's house. Upon capture, the seeds are all instantly moved into his store." ]]
              , div [ section ]
                    [ p [ sectionSubtitle ] [ text "Finish" ]
                    , p [ sectionText ] [ text "If a player cannot play because all six hollows are empty, the game ends and all the seeds on the other side of the board are captured by the other player." ]]
              , div [ section, class "text-center" ]
                    [ button [class "btn btn-link mancala-action", sectionBack, onClick(Back)] [text "Back"] ]
              ]]

getPlayingView : MancalaAPI.Model -> Html Msg
getPlayingView model = 
    div []
        [ div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
              [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [class "mancala-feedback", verticalStyle] [text (toString(MancalaAPI.getElementList (boardSize-1) model.board.list)) ]]]
              , div [ class "col-md-6", pxnull]
                    ((getPlayer2Row model) ++ (getPlayer1Row model))
              , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [class "mancala-feedback", verticalStyle] [text (toString(MancalaAPI.getElementList size model.board.list)) ]]]
              ]
        , getFeedbackView model.feedback
        , button [ class "col-md-2 btn btn-link mancala-hole", smallBoxStyle, player1Style, pxnull, onClick( Move (MancalaAPI.getBestMove model))]
                       [ p [][text "Make AI Move"] ] 
        ] 

getEndGameView : MancalaAPI.Model -> Html Msg 
getEndGameView model = 
    div []
        [ div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
              [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ h2 [class "mancala-feedback", verticalStyle] [text (toString(MancalaAPI.getElementList (boardSize-1) model.board.list)) ]]]
              , div [ class "col-md-6", pxnull]
                    ((getPlayer2Row model) ++ (getPlayer1Row model))
              , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ h2 [class "mancala-feedback", verticalStyle] [text (toString(MancalaAPI.getElementList size model.board.list)) ]]]
              ]
        , getFeedbackView model.feedback
        , getWinnerView model.board
        , div [ class "row text-center", style [ ("padding", "20px 15px 0") ] ]
              [ img [src "images/replay.png", replayStyle, onClick(Back)] []
              ]
        ] 

-- VIEW

view : MancalaAPI.Model -> Html Msg
view model = 
    case model.status of
        Menu -> 
            getMenuView model
        Rules ->
            getRulesView model
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
        ShowRules ->
            ( {model | status=Rules}, Cmd.none )
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

