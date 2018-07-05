module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "Muffin", Cmd.none )



-- UPDATE


type Msg
    = Trigger
    | NewWords (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Trigger ->
            ( model, getThoseWords )
        NewWords (Ok newWords)->
            ( newWords, Cmd.none )
        NewWords (Err _)->
            ( model, Cmd.none )


getThoseWords : Cmd Msg
getThoseWords =
    let 
        url =
            "http://localhost:3001/"
        request =
            Http.getString url 
    in 
        Http.send NewWords request


-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ 
           button
                [ class "pure-button pure-button-primary"
                , onClick Trigger
                ]
                [ text "get stuff" ]
                ,
                text model
        ]
