--module Main exposing (..)

--import Html exposing (..)
--import Html.Attributes exposing (class,style)
--import Html.Events exposing (onClick)
--import Html.App

---- API
---- Returns a sublist from io inclusive to ie exclusive
--getSubList : Int -> Int -> List a -> List a
--getSubList io ie list = 
--    case (io, ie, list) of
--        (_, _, []) ->
--            []
--        (_, 0, list) ->
--            []
--        (0, y, head::sublist) ->
--            head::getSubListFromIndex 0 (y-1) sublist
--        (x, y, head::sublist) ->
--            getSubListFromIndex (x-1) (y-1) sublist

---- STYLE

--size : Int
--size = 6

--boardSize : Int
--boardSize = 14

--value : Int
--value = 4

--boxWidth : Int
--boxWidth = 130 

--menuBoxStyle : Attribute Msg
--menuBoxStyle = style 
--    [ ("border-radius", "10px 10px 10px 10px")
--    , ("border", "9px double #ffffff")
--    , ("background-color", "#1abc9c")
--    , ("color", "#ffffff")
--    , ("height", "300px")
--    , ("width", "100%")
--    , ("padding", "40px 15px 0")
--    ]

--borderStyle : Attribute Msg
--borderStyle = style
--    [ ("border-top", "3px solid #ffffff") ]

--bigBoxStyle : Attribute Msg
--bigBoxStyle = style
--    [ ("height", toString(2 * boxWidth) ++ "px")
--    , ("color", "#ffffff")
--    , ("border", "5px solid #ffffff")
--    , ("padding-left", "0px")
--    , ("padding-right", "0px")
--    ]

--smallBoxStyle : Attribute Msg
--smallBoxStyle = style
--    [ ("height", toString(boxWidth) ++ "px")
--    , ("color", "#ffffff")
--    , ("border", "5px solid #ffffff")
--    ]

--player1Style : Attribute Msg
--player1Style = style
--    [ ("background-color", "#1abc9c") ]

--player2Style : Attribute Msg
--player2Style = style
--    [ ("background-color", "#a6aaa9") ]

--pxnull : Attribute Msg
--pxnull = style
--    [ ("padding-left", "0px")
--    , ("padding-right", "0px")
--    ]

---- MODEL

--type Turn = Player1 | Player2

--type Status = Menu | Playing

--type alias Board = 
--    { list : List Int
--    } 

--initBoard : Board
--initBoard =
--    { list =  (List.map (\n -> value) [1..size]) 
--           :: 0 
--           :: (List.map (\n -> value) [1..size])
--           :: 0 
--    }

--type alias Model =
--    { status : Status
--    , board : Board
--    , turn : Turn
--    }


--init : ( Model, Cmd Msg )
--init =
--    ( {status=Menu, board=initBoard, turn=Player1}, Cmd.none )



---- MESSAGES


--type Msg
--    = NoOp | Start | Move Int

--getPlayer2Row : Model ->  List (Html Msg)
--getPlayer2Row model = 
--    if model.turn == Player2
--    then
--        List.map 
--            (\(n,m) -> 
--                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull, onClick( Move m )]
--                    [ text (toString(n)) ]
--            ) 
--            (List.map2 (,) (List.reverse(getSubList (size+1) (boardSize-1) model.board.list )) (List.reverse(List.map (\n -> n) [(size+1)..(boardSize-2)])))
--    else
--        List.map 
--            (\n -> 
--                div [ class "col-md-2", smallBoxStyle, player2Style, pxnull]
--                    [ text (toString(n)) ]
--            ) 
--            (List.reverse(getSubList (size+1) (boardSize-1) model.board.list ))

--getPlayer1Row : Model ->  List (Html Msg)
--getPlayer1Row model = 
--    if model.turn == Player1
--    then
--        List.map 
--            (\(n,m) -> 
--                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull, onClick( Move m )]
--                    [ text (toString(n)) ]
--            ) 
--            (List.map2 (,) (getSubList 0 (size) model.board.list) (List.map (\n -> n) [0..(size-1)]))
--    else
--        List.map 
--            (\n -> 
--                div [ class "col-md-2", smallBoxStyle, player1Style, pxnull]
--                    [ text (toString(n)) ]
--            ) 
--            (getSubList 0 (size) model.board.list)

--getMenuView : Model -> Html Msg
--getMenuView model = 
--    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
--        [ div [ class "col-md-offset-4 col-md-4 text-center"] 
--              [ div [menuBoxStyle] 
--                    [ div [class "h2"]
--                          [text "Mancala"]
--                    , p [borderStyle] []
--                    , p [class "h3"] [text "Rules"]
--                    , p [class "h3", onClick (Start)] [text "Play"] ]]]

--getPlayingView : Model -> Html Msg
--getPlayingView model = 
--    div [ class "row text-center", style [ ("padding", "200px 15px 0") ] ]
--        [ div [ class "col-md-offset-2 col-md-1", pxnull] [ div [ bigBoxStyle, player2Style][ text (toString(getSeeds (boardSize-1) model.board.list)) ] ]
--        , div [ class "col-md-6", pxnull]
--              ((getPlayer2Row model) ++ (getPlayer1Row model))
--        , div [ class "col-md-1", pxnull] [ div [ bigBoxStyle, player1Style ][ text (toString(getSeeds size model.board.list)) ] ]
--        ]


---- VIEW

--view : Model -> Html Msg
--view model = 
--    case model.status of
--        Menu -> 
--            getMenuView model
--        Playing ->
--            getPlayingView model


---- UPDATE

--addOneMoreSeed : Int -> Int -> Int
--addOneMoreSeed position seeds =
--    if (position < (seeds % (2*(size+1))))
--    then 1
--    else 0

--updateList : Board -> Int -> Int -> List Int
--updateList board position seeds =
--    List.map
--        (\(n,m) -> 
--                if m/= position
--                then
--                    (n + (seeds // ((size+1)*2)) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds))
--                else
--                    ((seeds // ((size+1)*2)) + (addOneMoreSeed ((m - 1 - position) % boardSize) seeds)))
--        (List.map2 (,) list (List.map (\n -> n) [0..(boardSize-1)]))


--updateBoard : Board -> Int -> Int -> Board
--updateBoard board position seeds =
--    { list =
--        updateList board position seeds
--    }

--updateModel : Model -> Int -> Int -> Model
--updateModel model position seeds = 
--    {model | board=(updateBoard model.board position seeds)}

--getSeeds : Int -> List Int -> Int
--getSeeds x list =
--    case (x, list) of
--        (_, []) ->
--            -1
--        (0, head::tail) ->
--            head
--        (x, head::tail) ->
--            getSeeds (x-1) tail


----capture : Board -> position -> seeds ->
----capture b position seeds =
----    if getSeedsBoard b (getOpposite (position+seeds)) == 0 
----    && getSeedsBoard b (position+seeds) == 1 
----    && diffBoardSides (getOpposite (position+seeds)) (position+seeds)
----    -- Agregar algo respecto al turno
----    then removtSeedsBoard b (getOpposite (position+seeds)) (position+seeds)

--updateTurn : Turn -> Int -> Int -> Turn 
--updateTurn turn position seeds = 
--    case turn of
--        Player1 -> 
--            if ((position+seeds)%boardSize) == size
--            then Player1
--            else Player2
--        Player2 -> 
--            if ((position+seeds)%boardSize) == (boardSize-1)
--            then Player2
--            else Player1

--update : Msg -> Model -> ( Model, Cmd Msg )
--update msg model =
--    case msg of
--        NoOp ->
--            ( {model | status=Menu}, Cmd.none )
--        Start ->
--            ( {model | status=Playing}, Cmd.none )
--        Move position ->
--            ( (updateModel model position (getSeeds position model.board.p1List)), Cmd.none )
--            |> (\(n,m) -> ({n | turn=(updateTurn model.turn position (getSeeds position model.board.list))}, m))


---- SUBSCRIPTIONS


--subscriptions : Model -> Sub Msg
--subscriptions model =
--    Sub.none



---- MAIN


--main : Program Never
--main =
--    Html.App.program
--        { init = init
--        , view = view
--        , update = update
--        , subscriptions = subscriptions
--        }

