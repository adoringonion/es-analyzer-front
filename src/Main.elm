module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, disabled, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { input : String
    , state : State
    }


type State
    = Init
    | Waiting
    | Loaded Ranking
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | Send
    | Receive (Result Http.Error Ranking)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model
                | input = ""
                , state = Waiting
              }
            , Http.post
                { url = "https://es-analyzer.herokuapp.com/"
                , body = Http.multipartBody [ Http.stringPart "sentence" model.input ]
                , expect = Http.expectJson Receive rankingDecoder
                }
            )

        Receive (Ok ranking) ->
            ( { model | state = Loaded ranking }, Cmd.none )

        Receive (Err e) ->
            ( { model | state = Failed e }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Send ]
            [ textarea
                [ onInput Input
                , autofocus True
                , value model.input
                ]
                []
            , button
                [ disabled
                    ((model.state == Waiting)
                        || String.isEmpty (String.trim model.input)
                    )
                ]
                [ text "分析する" ]
            ]
        , case model.state of
            Init ->
                text ""

            Waiting ->
                text "分析中..."

            Loaded ranking ->
                ul []
                    [ li [] [ text "1位\u{3000}", text ranking.first ]
                    , li [] [ text "2位\u{3000}", text ranking.second ]
                    , li [] [ text "3位\u{3000}", text ranking.third ]
                    , li [] [ text "4位\u{3000}", text ranking.fourth ]
                    , li [] [ text "5位\u{3000}", text ranking.fifth ]
                    , li [] [ text "6位\u{3000}", text ranking.sixth ]
                    , li [] [ text "7位\u{3000}", text ranking.seventh ]
                    , li [] [ text "8位\u{3000}", text ranking.eighth ]
                    , li [] [ text "9位\u{3000}", text ranking.ninth ]
                    , li [] [ text "10位 ", text ranking.tenth ]
                    ]

            Failed e ->
                div [] [ text (Debug.toString e) ]
        ]



-- DATA


type alias Ranking =
    { first : String
    , second : String
    , third : String
    , fourth : String
    , fifth : String
    , sixth : String
    , seventh : String
    , eighth : String
    , ninth : String
    , tenth : String
    }


rankingDecoder : Decoder Ranking
rankingDecoder =
    succeed Ranking
        |> required "first" string
        |> required "second" string
        |> required "third" string
        |> required "fourth" string
        |> required "fifth" string
        |> required "sixth" string
        |> required "seventh" string
        |> required "eighth" string
        |> required "ninth" string
        |> required "tenth" string
