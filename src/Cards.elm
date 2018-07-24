module Cards exposing (..)

import Html
import Array
import Random


type alias Card =
    { suit : String
    , number : Int
    }


type alias Model =
    Array.Array Card


init : ( Model, Cmd Msg )
init =
    ( Array.empty, Random.generate HeresASeed (Random.int Random.minInt Random.maxInt) )


removeAt : Int -> Array.Array a -> Array.Array a
removeAt idx source =
    if idx == (Array.length source - 1) then
        Array.slice 0 idx source
    else
        Array.fromList
            (List.concat
                [ Array.toList (Array.slice 0 idx source)
                , Array.toList (Array.slice (idx + 1) (Array.length source) source)
                ]
            )


shuffleHelper : Random.Seed -> Array.Array a -> Array.Array a -> Array.Array a
shuffleHelper seed source result =
    if Array.isEmpty source then
        result
    else
        let
            indexGenerator =
                Random.int 0 ((Array.length source) - 1)

            ( index, nextSeed ) =
                Random.step indexGenerator seed

            valAtIndex =
                case Array.get index source of
                    Just value ->
                        value

                    Nothing ->
                        Debug.crash "oops a poopers"

            sourceWithoutIndex =
                removeAt index source
        in
            shuffleHelper nextSeed sourceWithoutIndex (Array.push valAtIndex result)


shuffleDem : Random.Seed -> Array.Array a -> Array.Array a
shuffleDem seed original =
    shuffleHelper seed original Array.empty


type Msg
    = HeresASeed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeresASeed randomInt ->
            ( getFullShuffledDeck randomInt, Cmd.none )


getFullShuffledDeck : Int -> Model
getFullShuffledDeck randomInt =
    getFittyTwo |> shuffleDem (Random.initialSeed randomInt)


getFittyTwo : Array.Array Card
getFittyTwo =
    let
        suits =
            [ "hearts", "spades", "diamonds", "clubs" ]

        numbers =
            (List.range 1 13)
    in
        List.map (\x -> List.map (\y -> { suit = x, number = y }) numbers) suits
            |> List.concat
            |> Array.fromList


view : Model -> Html.Html Msg
view model =
    Html.ul [] (Array.toList (Array.map (\x -> Html.li [] [ Html.text (toString x) ]) model))


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
