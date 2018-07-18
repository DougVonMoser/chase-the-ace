module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import WebSocket exposing (..)
import Json.Encode exposing (encode, object)  
import Json.Decode exposing (..)  
-- at, string, int, array, maybe-- dsf

type Stage = SitDown | Play

type alias Model =
    {
        scores: Array.Array (Int) -- array length of players, each representing their score
        , dealt: Array.Array (Maybe String) -- array length of the players
        , myIdx : Maybe Int -- index the current player sits in dealt array
        , dealerIdx : Int -- index of the dealer, as sitting in dealt
        , playerCount : Int -- the total number of players, (maybe redundant)
        , turnIdx : Int
        , serverHeadsUp: String
    }

initialModel : Model
initialModel = 
    {
        scores = fromList [0,0,0,0]
        , dealt = fromList [Just "12C", Just "9D", Just "8S", Just "3H"]
        -- , dealt = fromList [Nothing, Nothing, Nothing, Nothing]
        , myIdx = Nothing
        , dealerIdx = 0
        , playerCount = 4
        , turnIdx = 1
        , serverHeadsUp = "nohting yet"
    }

init : ( Model, Cmd Msg )
init =
    -- need to ask for my playerId
    ( initialModel, Cmd.none )


-- UPDATE

-- may not use these anymore?
type Msg
    = Deal -- regla ass command
    | Stay 
    | Switch
    | Incoming String
    | SelectSeat String

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case (message, model.myIdx) of
        (_, Just myIdx) ->
            -- this was dumb vv
            let 
                nextTurn = getNextTurn myIdx model.playerCount
                maybeMyCard = getIdx myIdx model.dealt
                allowedToSend = model.turnIdx == myIdx
            in 
                case message of
                    Deal ->
                        ( model, Cmd.none )
                    Stay ->
                        if allowedToSend then 
                            ( model, sendStay myIdx )
                        else 
                            ( model, Cmd.none )
                        -- ({model | turnIdx = (model.turnIdx + 1) % model.playerCount}, Cmd.none)
                    Switch -> 
                        if allowedToSend then 
                            ( model, sendSwitch myIdx nextTurn maybeMyCard model.dealt )
                        else 
                            ( model, Cmd.none )
                    Incoming payload ->
                        (handleIncoming payload model, Cmd.none)
                    _ ->
                        ( model, Cmd.none )

        (SelectSeat chosenSeatIdx, _) ->
            case String.toInt chosenSeatIdx of
                Ok gucci -> 
                    ( { model | myIdx = Just gucci }, Cmd.none )
                Err ljkasdjf->
                    ( model, Cmd.none )
        (_, Nothing) ->
            ( model, Cmd.none )




sendSwitch : Int -> Int -> Maybe String -> Array.Array (Maybe String) -> Cmd msg
sendSwitch myIdx nextTurn maybeMyCard dealt =
    let
        nextCard =
            getIdx nextTurn dealt
        updated = 
            Array.set myIdx nextCard dealt
        updatedAgain =
            Array.set nextTurn maybeMyCard updated
        gonnaSendThis = 
            Json.Encode.object [
                ("playerIdx", Json.Encode.int myIdx)
                , ("move", Json.Encode.string "Switch")
                -- shit, i need to stop using maybes in dealt. it fucks things up
                , ("newCards", Json.Encode.array (Array.map dealtMapper updatedAgain) )
            ]
        jsonstringified = 
            Json.Encode.encode 0 gonnaSendThis
    in
        send "ws://localhost:8080" jsonstringified

dealtMapper : Maybe String -> Json.Encode.Value
dealtMapper thing =
    case thing of
        Just val -> 
            Json.Encode.string val
        Nothing ->
            Json.Encode.string "uh oh"

sendStay : Int -> Cmd msg
sendStay playerIdx = 
    let 
        gonnaSendThis = 
            Json.Encode.object [
                ("playerIdx", Json.Encode.int playerIdx)
                , ("move", Json.Encode.string "Stay")
                , ("newCards", Json.Encode.array  (fromList [Json.Encode.string "13C", Json.Encode.string "9D", Json.Encode.string "8S", Json.Encode.string "3H"]) )
            ]
        jsonstringified = 
            Json.Encode.encode 0 gonnaSendThis
    in
        send "ws://localhost:8080" jsonstringified


-- type Move = Stay | Switch | Deal -- maybe? 

type alias PlayerAction = {
    playerIdx : Int
    , move : String --Move
    , newCards : Array.Array String
}


playerActionDecoder : Decoder PlayerAction 
playerActionDecoder =
    Json.Decode.map3 PlayerAction
        (at ["playerIdx"] int)
        (at ["move"] string)
        (at ["newCards"] ( array string ) )


handleIncoming : String -> Model -> Model
handleIncoming payload model =
    let something = 
        decodeString playerActionDecoder payload
    in
        case something of
            Ok value ->
                case value.move of
                    "Stay" ->
                        { model | 
                            turnIdx = getNextTurn model.turnIdx model.playerCount
                            , serverHeadsUp = "player " ++ toString model.turnIdx ++ " stayed!"
                        }
                    "Switch" ->
                        { model |
                            turnIdx = getNextTurn model.turnIdx model.playerCount
                            , serverHeadsUp = "player " ++ toString model.turnIdx ++ " switched!"
                            , dealt = Array.map Just value.newCards
                        }
                    _ ->
                        { model | serverHeadsUp = "idk what the heck happened" }

            _ ->
                { model | serverHeadsUp = "shit job decoding" }


getNextTurn : Int -> Int -> Int
getNextTurn current totalPlayers=
    (current + 1) % totalPlayers

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://localhost:8080" Incoming


-- VIEW


view : Model -> Html Msg
view model =
    case model.myIdx of
        Just myIdx ->
            let myCard =
                getIdx myIdx model.dealt
            in
                div [ class "container" ]
                    [ text model.serverHeadsUp
                        , viewStatus myIdx model
                        , viewCard myCard
                        , viewPlayOptions model myCard]
        Nothing ->
            input [onInput SelectSeat] []

viewStatus : Int -> Model -> Html Msg
viewStatus myIdx model =
    let turnStatus =
        if model.turnIdx == myIdx then
            div [] [text "my turn!"]
        else
            div [] [text "waiting!"]
    in
        div [] [
            turnStatus
            , viewDealCommand myIdx model.dealerIdx model.dealt
        ]

viewPlayOptions : Model -> Maybe String -> Html Msg
viewPlayOptions model myCard=
    div [] [
        button [onClick Stay ] [text "Stay"]
        , button [onClick Switch ] [text "Switch"]
         
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
        Array.get idx arr
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