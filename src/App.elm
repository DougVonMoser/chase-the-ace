module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import WebSocket exposing (..)
import Json.Encode exposing (encode, object)
import Json.Decode exposing (..)
import Random
import Cards exposing (Card, getFullShuffledDeck)
import Svg exposing (..)
import Svg.Attributes exposing (x, y, id, height, width)


type Stage
    = SitDown
    | Play


type alias Model =
    { -- scores: Array.Array Int -- array length of players, each representing their score
      players : Array.Array String --names of players! fun!
    , dealt : Array.Array Card -- array of cards
    , myIdx : Maybe Int -- index the current player sits in dealt array
    , myName : Maybe String -- index the current player sits in dealt array
    , dealerIdx : Int -- index of the dealer, as sitting in dealt
    , turnIdx : Int
    , serverHeadsUp : String
    }


initialModel : Model
initialModel =
    { -- scores = fromList [0,0,0,0]
      players = Array.empty
    , dealt = Array.empty
    , myIdx = Nothing
    , myName = Nothing
    , dealerIdx = 0
    , turnIdx = 1
    , serverHeadsUp = "nohting yet"
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Send Move
    | Incoming String
    | TypingName String
    | SelectSeat
    | RandomInt Int


type Move
    = Stay
    | Switch
    | Deal


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        playerCount =
            Array.length model.players
    in
        case ( message, model.myIdx ) of
            ( Incoming payload, _ ) ->
                let
                    ignoreMe =
                        Debug.log payload 1
                in
                    ( handleIncoming payload model playerCount, Cmd.none )

            ( Send move, Just myIdx ) ->
                let
                    nextTurn =
                        getNextTurn myIdx playerCount
                in
                    case move of
                        Deal ->
                            ( model, Random.generate RandomInt (Random.int Random.minInt Random.maxInt) )

                        Stay ->
                            -- if myIdx == model.dealerIdx then
                            -- end round
                            ( model, sendStay myIdx model.dealt )

                        Switch ->
                            -- if myIdx == model.dealerIdx then
                            -- grab from top card instead of next person
                            -- end round
                            case get myIdx model.dealt of
                                Just myCard ->
                                    ( model, sendSwitch myIdx nextTurn myCard model.dealt )

                                Nothing ->
                                    ( { model | serverHeadsUp = "error state?" }, Cmd.none )

            ( TypingName typings, _ ) ->
                ( { model | myName = Just typings }, Cmd.none )

            ( SelectSeat, _ ) ->
                case model.myName of
                    Just playerName ->
                        ( model, sendSeat playerName )

                    Nothing ->
                        ( model, Cmd.none )

            ( RandomInt randy, Just myIdx ) ->
                let
                    newModel =
                        addCardsToDealt model randy
                in
                    ( newModel, sendDeal newModel.dealt myIdx )

            ( _, Nothing ) ->
                ( model, Cmd.none )


addCardsToDealt : Model -> Int -> Model
addCardsToDealt model randy =
    let
        fittyTwo =
            Cards.getFullShuffledDeck randy

        justEnough =
            Array.slice 0 (Array.length model.players) fittyTwo

        ignoreMe =
            Debug.log (toString justEnough) 1
    in
        { model | dealt = justEnough }


sendDeal : Array.Array Card -> Int -> Cmd msg
sendDeal dealt myIdx =
    let
        gonnaSendThis =
            Json.Encode.object
                [ ( "playerIdx", Json.Encode.int myIdx )
                , ( "move", Json.Encode.string "Deal" )
                , ( "newCards", Json.Encode.array (decodeDealt dealt) )
                ]

        jsonstringified =
            Json.Encode.encode 0 gonnaSendThis

        ignoreMe =
            Debug.log (toString dealt) 1
    in
        sendStringToServer jsonstringified


sendSeat : String -> Cmd msg
sendSeat playerName =
    let
        gonnaSendThis =
            Json.Encode.object
                [ ( "move", Json.Encode.string "NewPlayer" )
                , ( "name", Json.Encode.string playerName )
                ]

        jsonstringified =
            Json.Encode.encode 0 gonnaSendThis
    in
        sendStringToServer jsonstringified


sendSwitch : Int -> Int -> Card -> Array.Array Card -> Cmd msg
sendSwitch myIdx nextTurnIdx myCard dealt =
    case get nextTurnIdx dealt of
        Just nextCard ->
            let
                updated =
                    Array.set myIdx nextCard dealt

                updatedAgain =
                    Array.set nextTurnIdx myCard updated

                gonnaSendThis =
                    Json.Encode.object
                        [ ( "playerIdx", Json.Encode.int myIdx )
                        , ( "move", Json.Encode.string "Switch" )

                        -- shit, i need to stop using maybes in dealt. it fucks things up
                        , ( "newCards"
                          , Json.Encode.array
                                (Array.map
                                    (\x ->
                                        Json.Encode.object
                                            [ ( "suit", Json.Encode.string x.suit )
                                            , ( "number", Json.Encode.int x.number )
                                            , ( "svgDisplayVal", Json.Encode.string x.svgDisplayVal )
                                            ]
                                    )
                                    updatedAgain
                                )
                          )
                        ]

                jsonstringified =
                    Json.Encode.encode 0 gonnaSendThis
            in
                sendStringToServer jsonstringified

        Nothing ->
            Cmd.none


decodeDealt : Array.Array Card -> Array.Array Json.Encode.Value
decodeDealt dealt =
    Array.map
        (\x ->
            Json.Encode.object
                [ ( "suit", Json.Encode.string x.suit )
                , ( "number", Json.Encode.int x.number )
                , ( "svgDisplayVal", Json.Encode.string x.svgDisplayVal )
                ]
        )
        dealt


sendStay : Int -> Array.Array Card -> Cmd msg
sendStay playerIdx dealt =
    let
        gonnaSendThis =
            Json.Encode.object
                [ ( "playerIdx", Json.Encode.int playerIdx )
                , ( "move", Json.Encode.string "Stay" )
                , ( "newCards"
                  , Json.Encode.array
                        (decodeDealt dealt)
                  )
                ]

        jsonstringified =
            Json.Encode.encode 0 gonnaSendThis
    in
        sendStringToServer jsonstringified


sendStringToServer : String -> Cmd msg
sendStringToServer =
    send "ws://localhost:8080"



-- type Move = Stay | Switch | Deal -- maybe?


type alias PlayerAction =
    { playerIdx : Int
    , move : String --Move
    , newCards : Array.Array Card
    }


playerActionDecoder : Decoder PlayerAction
playerActionDecoder =
    Json.Decode.map3 PlayerAction
        (at [ "playerIdx" ] int)
        (at [ "move" ] string)
        (at [ "newCards" ]
            (array
                (Json.Decode.map3 Card
                    (at [ "suit" ] string)
                    (at [ "number" ] int)
                    (at [ "svgDisplayVal" ] string)
                )
            )
        )


type alias MoveDecoder =
    { move : String
    }


playerMoveDecoder : Decoder MoveDecoder
playerMoveDecoder =
    Json.Decode.map MoveDecoder
        (at [ "move" ] string)



-- lets decode "move" property first. this fn should run for only select moves


handleIncoming : String -> Model -> Int -> Model
handleIncoming payload model playerCount =
    let
        newMove =
            decodeString playerMoveDecoder payload
    in
        case newMove of
            Ok value ->
                case value.move of
                    "NewPlayer" ->
                        assignNewPlayer payload model

                    _ ->
                        let
                            something =
                                decodeString playerActionDecoder payload
                        in
                            case ( something, model.myIdx ) of
                                ( Ok value, Just myIdx ) ->
                                    let
                                        playerThatMoved =
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
                                                { model
                                                    | turnIdx = getNextTurn model.turnIdx playerCount
                                                    , serverHeadsUp = playerThatMoved ++ " stayed!"
                                                }

                                            "Switch" ->
                                                { model
                                                    | turnIdx = getNextTurn model.turnIdx playerCount
                                                    , serverHeadsUp = playerThatMoved ++ " switched!"
                                                    , dealt = value.newCards
                                                }

                                            "Deal" ->
                                                { model
                                                    | dealt = value.newCards
                                                    , serverHeadsUp = playerThatMoved ++ " dealt!"
                                                }

                                            _ ->
                                                { model | serverHeadsUp = "idk what the heck happened" }

                                ( _, _ ) ->
                                    { model | serverHeadsUp = "shit job decoding doug" }

            _ ->
                model


type alias PlayerDecoder =
    { name : String
    }


newPlayerDecoder : Decoder PlayerDecoder
newPlayerDecoder =
    Json.Decode.map PlayerDecoder
        (at [ "name" ] string)


assignNewPlayer : String -> Model -> Model
assignNewPlayer payload model =
    let
        newPlayer =
            decodeString newPlayerDecoder payload
    in
        case newPlayer of
            Ok player ->
                let
                    newPlayersArr =
                        Array.push player.name model.players
                in
                    case model.myName of
                        Just myName ->
                            if player.name == myName then
                                { model
                                    | myIdx = Just ((Array.length newPlayersArr) - 1)
                                    , players = newPlayersArr
                                }
                            else
                                { model | players = newPlayersArr }

                        Nothing ->
                            { model | players = newPlayersArr }

            _ ->
                model


getNextTurn : Int -> Int -> Int
getNextTurn current totalPlayers =
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
            if Array.length model.dealt == 0 then
                div []
                    [ h4 [] [ Html.text "Waiting to start home skillet" ]
                    , viewDealCommand myIdx model.dealerIdx model.dealt
                    , showCurrentPlayers model.players
                    ]
            else
                let
                    myCard =
                        get myIdx model.dealt

                    myTurn =
                        model.turnIdx == myIdx

                    myDeal =
                        model.dealerIdx == myIdx
                in
                    div [ class "container" ]
                        [ Html.text model.serverHeadsUp
                        , viewStatus myIdx model
                        , viewCard myCard
                        , viewPlayOptions myTurn myDeal
                        ]

        Nothing ->
            div []
                [ div []
                    [ input [ onInput TypingName ] []
                    , button [ onClick SelectSeat ] [ Html.text "join" ]
                    ]
                , showCurrentPlayers model.players
                ]


showCurrentPlayers : Array.Array String -> Html Msg
showCurrentPlayers players =
    ul [] (Array.toList (Array.map (\x -> li [] [ Html.text x ]) players))


viewStatus : Int -> Model -> Html Msg
viewStatus myIdx model =
    let
        status =
            if model.turnIdx == myIdx then
                "my turn"
            else
                case get model.turnIdx model.players of
                    Just name ->
                        "waiting on " ++ name

                    Nothing ->
                        "waiting"
    in
        div [] [ Html.text status ]


viewPlayOptions : Bool -> Bool -> Html Msg
viewPlayOptions myTurn myDeal =
    case ( myTurn, myDeal ) of
        ( True, True ) ->
            div []
                [ button [ onClick (Send Stay) ] [ Html.text "Stay" ]
                , button [ onClick (Send Switch) ] [ Html.text "Random" ]
                ]

        ( True, False ) ->
            div []
                [ button [ onClick (Send Stay) ] [ Html.text "Stay" ]
                , button [ onClick (Send Switch) ] [ Html.text "Switch" ]
                ]

        ( False, _ ) ->
            div [] []


viewDealCommand : Int -> Int -> Array.Array Card -> Html Msg
viewDealCommand player dealer hands =
    let
        needsDealing =
            Array.length hands == 0
    in
        if player == dealer && needsDealing then
            button [ onClick (Send Deal) ] [ Html.text "lets get started!" ]
            -- else if player == dealer then
            --     Html.text "my deal, but dealt"
        else
            Html.text ""


getIdx : Int -> Array.Array (Maybe a) -> Maybe a
getIdx idx arr =
    let
        thing =
            Array.get idx arr
    in
        case thing of
            Just val ->
                val

            Nothing ->
                Nothing


viewCard : Maybe Card -> Html Msg
viewCard cardValue =
    case cardValue of
        Just card ->
            svg [ Svg.Attributes.height "245", Svg.Attributes.width "170" ]
                [ use
                    [ Svg.Attributes.xlinkHref ("#svg-cards_" ++ card.svgDisplayVal)
                    , x "0"
                    , y "0"
                    ]
                    []
                ]

        Nothing ->
            Html.text "aint no card yet"
