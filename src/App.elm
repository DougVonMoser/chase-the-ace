module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

type alias Model =
    {
        scores: Array.Array (Int) -- array length of players, each representing their score
        , dealt: Array.Array (Maybe String) -- array length of the players
        , myIdx : Int -- index the current player sits in dealt array
        , dealerIdx : Int -- index of the dealer, as sitting in dealt
        , playerCount : Int -- the total number of players, (maybe redundant)
        , turnIdx : Int
    }

initialModel : Model
initialModel = 
    {
        scores = fromList [0,0,0,0]
        , dealt = fromList [Just "12C", Just "9D", Just "8S", Just "3H"]
        -- , dealt = fromList [Nothing, Nothing, Nothing, Nothing]
        , myIdx = 1
        , dealerIdx = 0
        , playerCount = 4
        , turnIdx = 1
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- 
-- UPDATE


type Msg
    = Deal -- regla ass command
    | Stay Int -- myIdx
    | Switch (Int, Maybe String) --myIdx and card

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    if model.turnIdx == model.myIdx then
        case message of
            Deal ->
                ( model, Cmd.none )
            Stay myIdx->
                    ({model | turnIdx = (model.turnIdx + 1) % model.playerCount}, Cmd.none)
            _ ->
                ( model, Cmd.none )
    else
        ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
    let myCard =
        getIdx model.myIdx model.dealt
    in
        div [ class "container" ]
            [ viewStatus model
                , viewCard myCard
                , viewPlayOptions model myCard]

viewStatus : Model -> Html Msg
viewStatus model =
    let turnStatus =
        if model.turnIdx == model.myIdx then
            div [] [text "my turn!"]
        else
            div [] [text "waiting!"]
    in
        div [] [
            turnStatus
            , viewDealCommand model.myIdx model.dealerIdx model.dealt
        ]

viewPlayOptions : Model -> Maybe String -> Html Msg
viewPlayOptions model myCard=
    div [] [
        button [onClick (Stay model.myIdx) ] [text "Stay"]
        , button [onClick (Switch (model.myIdx, myCard))] [text "Switch"]
         
    ]


viewDealCommand : Int -> Int -> Array.Array (Maybe String) -> Html Msg
viewDealCommand player dealer hands = 
    let needsDealing =
        List.all ( (==) Nothing ) (toList hands)
    in
        if player == dealer && needsDealing then
            button [onClick Deal] [text "im da dealer"]
        else if player == dealer then
            text "my deal, but dealt"
        else
            text "not my deal"



getIdx : Int -> Array.Array (Maybe a) -> Maybe a
getIdx idx arr =
    let thing = 
        get idx arr
    in
        case thing of
            Just val ->
                val
            Nothing ->
                Nothing

viewCard : Maybe String -> Html Msg
viewCard cardValue = 
    case cardValue of
        Just card ->
            text card
        Nothing ->
            text "aint no card yet"