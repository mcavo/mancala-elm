module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class,style)
import Html.Events exposing (onClick)
import Html.App

-- STYLE

boxWidth : Int
boxWidth = 130 

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
    [ ("height", toString(2 * boxWidth) ++ "px")
    , ("color", "#ffffff")
    , ("border", "5px solid #ffffff")
    , ("padding-left", "0px")
    , ("padding-right", "0px")
    ]

smallBoxStyle : Attribute Msg
smallBoxStyle = style
    [ ("height", toString(boxWidth) ++ "px")
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

type Status = Menu | Playing

type alias Board = 
    { p1Points : Int
    , p2Points : Int
    , p1List : List Int
    , p2List : List Int
    } 

initBoard : Int -> Int -> Board
initBoard size value=
    { p1Points = 0
    , p2Points = 0
    , p1List =
        List.map (\n -> n) [1..size]
    , p2List =
        List.map (\n -> n) [1..size]
    }

type alias Model =
    { status : Status
    , board : Board
--  turn : Turn,
    }


init : ( Model, Cmd Msg )
init =
    ( {status=Menu, board=initBoard 6 4}, Cmd.none )



-- MESSAGES


type Msg
    = NoOp | Start

getPlayer1Row : Model ->  List (Html Msg)
getPlayer1Row model = List.map 
    (\n -> 
        div [ class "col-md-2", smallBoxStyle, player1Style, pxnull]
            [ text (toString(n)) ]
    ) 
    (List.reverse(model.board.p1List))

getPlayer2Row : Model ->  List (Html Msg)
getPlayer2Row model = List.map 
    (\n -> 
        div [ class "col-md-2", smallBoxStyle, player2Style, pxnull]
            [ text (toString(n)) ]
    ) 
    model.board.p2List



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
        [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player1Style][ text (toString(model.board.p1Points)) ] ]
        , div [ class "col-md-6", pxnull]
              ((getPlayer1Row model) ++ (getPlayer2Row model))
        , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player2Style ][ text (toString(model.board.p2Points)) ] ]
        ]


-- VIEW

view : Model -> Html Msg
view model = 
    case model.status of
        Menu -> 
            getMenuView model
        Playing ->
            getPlayingView model


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( {model | status=Menu}, Cmd.none )
        Start ->
            ( {model | status=Playing}, Cmd.none )



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

