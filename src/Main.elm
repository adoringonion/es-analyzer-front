module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


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
    | Loaded CompanyRanking
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
    | Receive (Result Http.Error CompanyRanking)


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
                , expect = Http.expectJson Receive userDecoder
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
                , placeholder "GitHub name"
                , value model.input
                ]
                []
            , button
                [ disabled
                    ((model.state == Waiting)
                        || String.isEmpty (String.trim model.input)
                    )
                ]
                [ text "Submit" ]
            ]
        , case model.state of
            Init ->
                text ""

            Waiting ->
                text "Waiting..."

            Loaded ranking ->
                ul []
                    [ li [] [ text ranking.first ]
                    , li [] [ text ranking.second ]
                    ]

            Failed e ->
                div [] [ text (Debug.toString e) ]
        ]



-- DATA


type alias CompanyRanking =
    { first : String
    , second : String
    , third : String
    , fourth : String
    }



-- , fifth : String
-- , sixth : String
-- , seventh : String
-- , eighth : String
-- , ninth : String
-- , tenth : String
-- }


userDecoder : Decoder CompanyRanking
userDecoder =
    D.map4 CompanyRanking
        (D.field "first" D.string)
        (D.field "second" D.string)
        (D.field "third" D.string)
        (D.field "fourth" D.string)



-- (D.field "fifth" D.string)
-- (D.field "sixth" D.string)
-- (D.field "seventh" D.string)
-- (D.field "eighth" D.string)
-- (D.field "ninth" D.string)
-- (D.field "tenth" D.string)
