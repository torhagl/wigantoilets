module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Regex
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Form.Input as Input
import Bootstrap.Card as Card
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
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
        , Grid.row []
            [ Grid.col []
                [ div [ style [ ( "margin-top", "2em" ), ( "margin-bottom", "0.5em" ) ] ]
                    [ Input.text
                        [ Input.placeholder "Search for a town or something!"
                        , Input.onInput Input
                        ]
                    ]
                ]
            ]
        , Grid.row [ Row.middleLg ] <| List.map createToiletCard <| regexTowns model.input model.db.features
        ]


createToiletCard : Feature -> Grid.Column Msg
createToiletCard toilet =
    Grid.col [ Col.md6 ]
        [ Card.config [ Card.outlineInfo ]
            |> Card.headerH5 [] [ text toilet.purpleProperties.lOCATION ]
            |> Card.imgBottom [ src toilet.purpleProperties.lINKTOPHOTO, style [ ( "width", "100%" ), ( "height", "40%" ), ( "float", "left" ) ], alt "Photo of the toilet." ] []
            |> Card.block []
                [ Card.text [] [ text <| "Address: " ++ toilet.purpleProperties.sTREET ++ ", " ++ toilet.purpleProperties.tOWN ]
                , Card.text [] [ text <| "Managed by: " ++ toilet.purpleProperties.mANAGEDBY ]
                , Card.text [] [ text <| "Status: " ++ toilet.purpleProperties.sTATUS ]
                , Card.text [] [ text <| "Used: " ++ toilet.purpleProperties.uSED ]
                , Card.text []
                    [ if toilet.purpleProperties.sIZE == " " then
                        text <| "Size: Not specified."
                      else
                        text <| "Size: " ++ toilet.purpleProperties.sIZE
                    ]
                , Card.text []
                    [ if toilet.purpleProperties.cOMMENTS == " " then
                        text <| "Comments: None"
                      else
                        text <| "Comments: " ++ toilet.purpleProperties.cOMMENTS
                    ]
                ]
            |> Card.view
        ]


searchFilterTowns : String -> Welcome -> Html Msg
searchFilterTowns input db =
    div [] (List.map (\y -> div [] [ Html.text <| toSentenceCase y.purpleProperties.tOWN, Html.text <| toSentenceCase y.purpleProperties.cOMMENTS ]) <| regexTowns input db.features)


regexTowns : String -> List Feature -> List Feature
regexTowns input toilets =
    List.filter (\y -> Regex.contains (Regex.caseInsensitive <| Regex.regex <| "^" ++ input) y.purpleProperties.tOWN) toilets
