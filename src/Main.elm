module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, disabled, href, id, value)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Url.Builder as UB exposing (crossOrigin, string)


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
                        "あなたのESに似ている企業は\n1位 " ++ ranking.first ++ "\n2位 " ++ ranking.second ++ "\n3位 " ++ ranking.third ++ "\n4位 " ++ ranking.fourth ++ "\n5位 " ++ ranking.fifth ++ "\n6位 " ++ ranking.sixth ++ "\n7位 " ++ ranking.seventh ++ "\n8位 " ++ ranking.eighth ++ "\n9位 " ++ ranking.ninth ++ "\n10位 " ++ ranking.tenth

                    url =
                        crossOrigin "http://twitter.com" [ "share" ] [ UB.string "url" "https://es-analyzer.com/", UB.string "text" tweetText ]
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
                    , a [ id "tweetButton", href url ] [ text "結果をツイートする" ]
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
        |> required "first" Json.Decode.string
        |> required "second" Json.Decode.string
        |> required "third" Json.Decode.string
        |> required "fourth" Json.Decode.string
        |> required "fifth" Json.Decode.string
        |> required "sixth" Json.Decode.string
        |> required "seventh" Json.Decode.string
        |> required "eighth" Json.Decode.string
        |> required "ninth" Json.Decode.string
        |> required "tenth" Json.Decode.string
