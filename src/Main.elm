module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class,style)
import Html.App

-- STYLE

menuBoxStyle = style 
                [ ("border-radius", "10px 10px 10px 10px")
                , ("border", "9px double #ffffff")
                , ("background-color", "#1abc9c")
                , ("color", "#ffffff")
                , ("height", "300px")
                , ("width", "100%")
                , ("padding", "40px 15px 0")
                ]

borderStyle = style
                [ ("border-top", "3px solid #ffffff") ]


-- MODEL

type Turn = Player1 | Player2

type Status = Menu | Playing

-- type Board = ?????

type alias Model =
    {
    status : Status
--  board : Board,
--  turn : Turn,
    }


init : ( Model, Cmd Msg )
init =
    ( {status=Menu}, Cmd.none )



-- MESSAGES


type Msg
    = NoOp | Start


getMenuView : Html Msg
getMenuView = 
    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-4 col-md-4 text-center"] 
              [ div [menuBoxStyle] 
                    [ div [class "h2"]
                          [text "Mancala"]
                    , p [borderStyle] []
                    , p [class "h3"] [text "Rules"]
                    , p [class "h3"] [text "Play"] ]]]

getPlayingView : Html Msg
getPlayingView =
    div [ class "row", style [ ("padding", "200px 15px 0") ] ]
        [ div [ class "col-md-offset-1 col-md-10 text-center"] 
              [ div [menuBoxStyle] 
                    [ div [class "h2"]
                          [text "Mancala"]
                    , p [borderStyle] []
                    , p [class "h3"] [text "Rules"]
                    , p [class "h3"] [text "Play"] ]]]

-- VIEW

view : Model -> Html Msg
view model = 
    case model.status of
        Menu -> 
            getMenuView
        Playing ->
            getMenuView


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
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

