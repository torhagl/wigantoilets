module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Regex
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import String.Extra exposing (..)
import Json.Decode exposing (decodeString)
import JsonDecoder exposing (welcome, Welcome, Feature)
import Http


getJson : String -> Cmd Msg
getJson filepath =
    Http.send InsertDB <| Http.get filepath welcome


main =
    Html.program
        { init = init, view = view, update = update, subscriptions = subscriptions }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { db = { purpleType = "", features = [] }, input = "" }, getJson "/plswork.json" )



-- MODEL


type alias Model =
    { input : String
    , db : Welcome
    }



-- UPDATE


type Msg
    = Input String
    | InsertDB (Result Http.Error Welcome)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | input = s }, Cmd.none )

        InsertDB (Ok result) ->
            ( { model | db = result }, Cmd.none )

        InsertDB (Err _) ->
            ( { model | db = { features = [], purpleType = "Some error." } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div [ style [ ( "margin-top", "2em" ) ] ]
            [ Input.text
                [ Input.placeholder "Start typing!"
                , Input.onInput Input
                ]
            ]
        , div [ style [ ( "margin-top", "0.5em" ) ] ]
            [ if model.input == "" then
                text "Results come here!"
              else
                searchFilterTowns model.input model.db

            --text getJson
            ]
        ]


searchFilterTowns : String -> Welcome -> Html Msg
searchFilterTowns input db =
    div [] (List.map (\y -> div [] [ Html.text <| toSentenceCase y.purpleProperties.tOWN ]) <| regexTowns input db.features)


regexTowns : String -> List Feature -> List JsonDecoder.Feature
regexTowns input toilets =
    List.filter (\y -> Regex.contains (Regex.caseInsensitive <| Regex.regex <| "^" ++ input) y.purpleProperties.tOWN) toilets
