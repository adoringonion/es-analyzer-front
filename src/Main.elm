module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, disabled, href, id, value)
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
                | state = Waiting
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
    div [ id "container" ]
        [ h1 [ id "title", class "center" ]
            [ text "ES-ANALYZER" ]
        , div [ id "description", class "center" ]
            [ p [ id "desc" ]
                [ text "約500社分のエントリーシートを学習したAI（機械学習）が" ]
            , p [ id "desc2" ]
                [ text "あなたの文章がどの企業のエントリーシートに似ているか分析します" ]

            -- , ul [ id "heed" ]
            --     [ li [] [ text "結果を本気にしないでください" ]
            --     , li [] [ text "分析結果は毎回少し変動します" ]
            --     , li [] [ text "このアプリは複数の就活サイトに掲載されているエントリーシートを分析した結果をもとに作られています" ]
            --     ]
            ]
        , case model.state of
            Init ->
                Html.form [ onSubmit Send, id "inputForm", class "center" ]
                    [ textarea
                        [ onInput Input
                        , value model.input
                        ]
                        []
                    , p []
                        [ button
                            [ disabled
                                ((model.state == Waiting)
                                    || String.isEmpty (String.trim model.input)
                                )
                            , id "analyzeButton"
                            ]
                            [ text "分析する" ]
                        ]
                    ]

            Waiting ->
                div [ id "loader", class "center" ]
                    [ text "分析中…" ]

            Loaded ranking ->
                let
                    tweetText =
                        "http://twitter.com/share?url="
                            ++ "https://es-analyzer.com/"
                            ++ "&text="
                            ++ "あなたのESに似ている企業は%0a%0a"
                            ++ "1位%20"
                            ++ ranking.first
                            ++ "%0a"
                            ++ "2位%20"
                            ++ ranking.second
                            ++ "%0a"
                            ++ "3位%20"
                            ++ ranking.third
                            ++ "%0a"
                            ++ "4位%20"
                            ++ ranking.fourth
                            ++ "%0a"
                            ++ "5位%20"
                            ++ ranking.fifth
                            ++ "%0a"
                            ++ "6位%20"
                            ++ ranking.sixth
                            ++ "%0a"
                            ++ "7位%20"
                            ++ ranking.seventh
                            ++ "%0a"
                            ++ "8位%20"
                            ++ ranking.eighth
                            ++ "%0a"
                            ++ "9位%20"
                            ++ ranking.ninth
                            ++ "%0a"
                            ++ "10位%20"
                            ++ ranking.tenth
                            ++ "%0a"
                in
                div [ id "result", class "center" ]
                    [ Html.form [ onSubmit Send, id "inputForm", class "center" ]
                        [ textarea
                            [ onInput Input
                            , value model.input
                            ]
                            []
                        , p []
                            [ button
                                [ disabled
                                    ((model.state == Waiting)
                                        || String.isEmpty (String.trim model.input)
                                    )
                                , id "analyzeButton"
                                ]
                                [ text "分析する" ]
                            ]
                        ]
                    , ul [ id "ranking" ]
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
                    , a [ id "tweetButton", href tweetText ] [ text "結果をツイートする" ]
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
