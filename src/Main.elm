module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser exposing ((</>), Parser, oneOf, parse)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Route
    = Top
    | About
    | NotFound


type alias Model =
    { key : Nav.Key
    , route : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key Top, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    parseUrl url
            in
            ( { model | route = route }
            , Cmd.none
            )



-- ROUTE


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map About (Url.Parser.s "about")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    let
        parsed =
            parse routeParser url
    in
    case parsed of
        Just route ->
            route

        Nothing ->
            NotFound



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ ul []
            [ viewLink "/"
            , viewLink "/about"
            , viewLink "/notfound"
            ]
        , case model.route of
            Top ->
                div [] [ text "Top" ]

            About ->
                div [] [ text "About" ]

            NotFound ->
                div [] [ text "NotFound" ]
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
