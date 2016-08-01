module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class,style)
import Html.Events exposing (onClick)
import Html.App

-- API

getSubListFromIndex : Int -> List a -> List a
getSubListFromIndex el list = 
    case (el, list) of
        (0, list) ->
            list
        (x, []) ->
            []
        (x, head::list) ->
            getSubListFromIndex (x-1) list

-- STYLE

size : Int
size = 6

boardSize : Int
boardSize = 14

value : Int
value = 4

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

initBoard : Board
initBoard =
    { p1Points = 0
    , p2Points = 0
    , p1List =
        List.map (\n -> value) [1..size]
    , p2List =
        List.map (\n -> value) [1..size]
    }

type alias Model =
    { status : Status
    , board : Board
--  turn : Turn,
    }


init : ( Model, Cmd Msg )
init =
    ( {status=Menu, board=initBoard}, Cmd.none )



-- MESSAGES


type Msg
    = NoOp | Start | MoveP1 Int | MoveP2 Int

getPlayer1Row : Model ->  List (Html Msg)
getPlayer1Row model = List.map 
    (\(n,m) -> 
        div [ class "col-md-2", smallBoxStyle, player1Style, pxnull, onClick( MoveP2 m )]
            [ text (toString(n) ++ " " ++ toString(m)) ]
    ) 
    (List.map2 (,) (List.reverse(model.board.p2List)) (List.reverse(List.map (\n -> n) [0..(size-1)])))

getPlayer2Row : Model ->  List (Html Msg)
getPlayer2Row model = List.map 
    (\(n,m) -> 
        div [ class "col-md-2", smallBoxStyle, player2Style, pxnull, onClick( MoveP1 m )]
            [ text (toString(n) ++ " " ++ toString(m)) ]
    ) 
    (List.map2 (,) model.board.p1List (List.map (\n -> n) [0..(size-1)]))



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
        [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player1Style][ text (toString(model.board.p2Points)) ] ]
        , div [ class "col-md-6", pxnull]
              ((getPlayer1Row model) ++ (getPlayer2Row model))
        , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player2Style ][ text (toString(model.board.p1Points)) ] ]
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

updateRow : Board -> List Int -> List Int -> Int -> Int -> List Int
updateRow board list positions position seeds = List.map 
    (\(n,m) -> 
        if m /= position
        then
            (n + (seeds // ((size+1)*2)) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds))
        else
            ((seeds // ((size+1)*2)) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds)))
    (List.map2 (,) list (List.map (\n -> n) positions))

addOneMoreSeed : Int -> Int -> Int
addOneMoreSeed position seeds =
    if (position < (seeds % (2*(size+1))))
    then 1
    else 0

updateBoard : Board -> Int -> Int -> Board
updateBoard board position seeds =
    { p1Points 
        = board.p1Points 
        + (seeds // boardSize) 
        + (addOneMoreSeed ((size - 1 - position) % boardSize) seeds) 
    , p2Points
        = board.p2Points
        + (seeds // boardSize)
        + (addOneMoreSeed ((boardSize - 2 - position) % boardSize) seeds)
    , p1List = 
        updateRow board board.p1List [0..(size-1)] position seeds
    , p2List =
        updateRow board board.p2List [(size+1)..(boardSize - 2)] position seeds
    }

updateModel : Model -> Int -> Int -> Model
updateModel model position seeds = 
    {model | board=(updateBoard model.board position seeds)}

getSeeds : Int -> List Int -> Int
getSeeds x list =
    case (x, list) of
        (_, []) ->
            -1
        (0, head::tail) ->
            head
        (x, head::tail) ->
            getSeeds (x-1) tail


--capture : Board -> position -> seeds ->
--capture b position seeds =
--    if getSeedsBoard b (getOpposite (position+seeds)) == 0 
--    && getSeedsBoard b (position+seeds) == 1 
--    && diffBoardSides (getOpposite (position+seeds)) (position+seeds)
--    -- Agregar algo respecto al turno
--    then removtSeedsBoard b (getOpposite (position+seeds)) (position+seeds)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( {model | status=Menu}, Cmd.none )
        Start ->
            ( {model | status=Playing}, Cmd.none )
        MoveP1 position ->
            ( (updateModel model position (getSeeds position model.board.p1List)), Cmd.none )
        MoveP2 position ->
            ( (updateModel model (position + size + 1) (getSeeds position model.board.p2List)), Cmd.none )


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

