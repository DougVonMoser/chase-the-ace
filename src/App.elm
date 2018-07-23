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
        stage: Stage
        -- scores: Array.Array Int -- array length of players, each representing their score
        , players : Array.Array (String) --names of players! fun!
        , dealt: Array.Array (Maybe String) -- array length of the players
        , myIdx : Maybe Int -- index the current player sits in dealt array
        , myName : Maybe String -- index the current player sits in dealt array
        , dealerIdx : Int -- index of the dealer, as sitting in dealt
        , playerCount : Int -- the total number of players, (maybe redundant)
        , turnIdx : Int
        , serverHeadsUp: String
    }

initialModel : Model
initialModel = 
    {
        stage = SitDown
        -- scores = fromList [0,0,0,0]
        , players = Array.empty
        , dealt =  fromList [Nothing , Nothing , Nothing , Nothing ]
        -- , dealt =  fromList [Just "12C",Just "9D",Just "8S",Just "3H"]
        -- , dealt = Just ( fromList ["12C", "9D", "8S", "3H"] )
        -- , dealt = Nothing
        , myIdx = Nothing
        , myName = Nothing
        , dealerIdx = 0
        , playerCount = 4
        , turnIdx = 1
        , serverHeadsUp = "nohting yet"
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


-- UPDATE

-- may not use these anymore?
type Msg
    = Send Move
    | Incoming String
    | TypingName String
    | SelectSeat

type Move
    = Stay
    | Switch 
    | Deal

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case (message, model.myIdx) of
        (Incoming payload, _) ->
            let ignoreMe = 
                Debug.log payload 1
            in
                (handleIncoming payload model, Cmd.none)
        (Send move, Just myIdx) ->
            let 
                nextTurn = getNextTurn myIdx model.playerCount
                maybeMyCard = getIdx myIdx model.dealt
            in 
                case move of
                    Deal ->
                        ( model, sendDeal model myIdx )
                    Stay ->
                        ( model, sendStay myIdx )
                    Switch -> 
                        ( model, sendSwitch myIdx nextTurn maybeMyCard model.dealt )

        (TypingName typings, _) ->
            ( { model | myName = Just typings }, Cmd.none )
        (SelectSeat, _) ->
            case model.myName of
                Just playerName ->
                    ( model, sendSeat playerName )
                Nothing ->
                    ( model, Cmd.none )
        (_, Nothing) ->
            ( model, Cmd.none )


sendDeal : Model -> Int -> Cmd msg
sendDeal model myIdx =
   let 
        gonnaSendThis = 
            Json.Encode.object [
                ("playerIdx", Json.Encode.int myIdx)
                , ("move", Json.Encode.string "Deal")
                , ("newCards", Json.Encode.array  (fromList [Json.Encode.string "13C", Json.Encode.string "9D", Json.Encode.string "8S", Json.Encode.string "3H"]) )
            ]
        jsonstringified = 
            Json.Encode.encode 0 gonnaSendThis
    in
        sendStringToServer jsonstringified

sendSeat : String -> Cmd msg
sendSeat playerName =
    let gonnaSendThis = 
        Json.Encode.object [
            ("move", Json.Encode.string "NewPlayer")
            , ("name", Json.Encode.string playerName)
        ]
        jsonstringified = 
            Json.Encode.encode 0 gonnaSendThis
    in
        sendStringToServer jsonstringified

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
        sendStringToServer jsonstringified

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
        sendStringToServer jsonstringified

sendStringToServer : String -> Cmd msg
sendStringToServer = send "ws://localhost:8080"

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


type alias MoveDecoder = {
    move: String
}

playerMoveDecoder : Decoder MoveDecoder
playerMoveDecoder =
    Json.Decode.map MoveDecoder
        (at ["move"] string)

-- lets decode "move" property first. this fn should run for only select moves
handleIncoming : String -> Model -> Model
handleIncoming payload model =


    let newMove = 
            decodeString playerMoveDecoder payload
    in 
        case newMove of
            Ok value ->  
                case value.move of
                    "NewPlayer" ->
                        assignNewPlayer payload model
                    _ -> 
                        let something = 
                            decodeString playerActionDecoder payload
                        in
                            case (something, model.myIdx) of
                                (Ok value, Just myIdx) ->
                                    let playerThatMoved = 
                                        if (myIdx == value.playerIdx) then
                                            "You"
                                        else 
                                            case Array.get value.playerIdx model.players of
                                                Just player ->
                                                    player ++ " "
                                                Nothing -> 
                                                    "Player " ++ toString value.playerIdx
                                    in
                                        case value.move of
                                            "Stay" ->
                                                { model | 
                                                    turnIdx = getNextTurn model.turnIdx model.playerCount
                                                    , serverHeadsUp = playerThatMoved ++ " stayed!"
                                                }
                                            "Switch" ->
                                                { model |
                                                    turnIdx = getNextTurn model.turnIdx model.playerCount
                                                    , serverHeadsUp = playerThatMoved ++ " switched!"
                                                    , dealt = Array.map Just value.newCards
                                                }

                                            "Deal" ->
                                                { model | 
                                                    -- dealt = fromList [Just "12C",Just "9D",Just "8S",Just "3H"]
                                                    dealt = Array.map Just value.newCards
                                                    , serverHeadsUp = playerThatMoved ++ " dealt!"
                                                }
                                            _ ->
                                                { model | serverHeadsUp = "idk what the heck happened" }

                                (_, _) ->
                                    { model | serverHeadsUp = "shit job decoding doug" }
            _ ->
                model 


type alias PlayerDecoder = {
    name : String
}

newPlayerDecoder : Decoder PlayerDecoder
newPlayerDecoder =
    Json.Decode.map PlayerDecoder
        (at ["name"] string)

assignNewPlayer : String -> Model -> Model
assignNewPlayer payload model =
    let newPlayer = 
        decodeString newPlayerDecoder payload
    in
        case newPlayer of
            Ok player ->
                let newPlayersArr =
                    Array.push player.name model.players 
                in
                    case model.myName of
                        Just myName ->
                            if player.name == myName then
                                {model | myIdx = Just ((Array.length newPlayersArr) - 1)
                                , players = newPlayersArr}
                            else 
                                {model | players = newPlayersArr}
                        Nothing -> 
                            {model | players = newPlayersArr}
            _ ->
                model


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
                myTurn = 
                model.turnIdx == myIdx 
            in
                div [ class "container" ]
                    [ text model.serverHeadsUp
                        , viewStatus myIdx model
                        , viewCard myCard
                        , viewPlayOptions myTurn]
        Nothing ->
            div [] [
                div [] [
                    input [onInput TypingName] []
                    , button [onClick SelectSeat ] [text "join"]
                ]
                , showCurrentPlayers model.players
                
            ]


showCurrentPlayers : Array.Array (String) -> Html Msg
showCurrentPlayers players =
    ul [] (Array.toList (Array.map (\x -> li [] [text x]) players) ) 
    

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

viewPlayOptions : Bool -> Html Msg
viewPlayOptions myTurn =
    if myTurn then 
        div [] [
            button [onClick (Send Stay) ] [text "Stay"]
            , button [onClick (Send Switch) ] [text "Switch"]
            
        ] 
    else
        div [] []



viewDealCommand : Int -> Int -> Array.Array (Maybe String) -> Html Msg
viewDealCommand player dealer hands = 
    let needsDealing =
        List.all ( (==) Nothing ) (toList hands)
    in
        if player == dealer && needsDealing then
            button [onClick (Send Deal)] [text "im da dealer"]
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