-- To decode the JSON data, add this file to your project, run
--
--     elm-package install NoRedInk/elm-decode-pipeline
--
-- add these imports
--
--     import Json.Decode exposing (decodeString)
--     import QuickType exposing (welcome)
--
-- and you're off to the races with
--
--     decodeString welcome myJsonString


module JsonDecoder
    exposing
        ( Welcome
        , welcomeToString
        , welcome
        , Feature
        , Properties
        , Geometry
        )

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import List exposing (map)


type alias Welcome =
    { features : List Feature
    , purpleType : String
    }


type alias Feature =
    { purpleGeometry : Geometry
    , purpleProperties : Properties
    , purpleType : String
    }


type alias Properties =
    { cOMMENTS : String
    , hUBLOCALITY : String
    , lINKTOPHOTO : String
    , lOCATION : String
    , mANAGEDBY : String
    , oBJECTID : Int
    , pART : String
    , sIZE : String
    , sTATUS : String
    , sTREET : String
    , tOWN : String
    , uRN : String
    , uSED : String
    , wARD : String
    }


type alias Geometry =
    { coordinates : List Float
    , purpleType : String
    }



-- decoders and encoders


welcomeToString : Welcome -> String
welcomeToString r =
    Jenc.encode 0 (encodeWelcome r)


welcome : Jdec.Decoder Welcome
welcome =
    Jpipe.decode Welcome
        |> Jpipe.required "features" (Jdec.list feature)
        |> Jpipe.required "type" Jdec.string


encodeWelcome : Welcome -> Jenc.Value
encodeWelcome x =
    Jenc.object
        [ ( "features", makeListEncoder encodeFeature x.features )
        , ( "type", Jenc.string x.purpleType )
        ]


feature : Jdec.Decoder Feature
feature =
    Jpipe.decode Feature
        |> Jpipe.required "geometry" geometry
        |> Jpipe.required "properties" properties
        |> Jpipe.required "type" Jdec.string


encodeFeature : Feature -> Jenc.Value
encodeFeature x =
    Jenc.object
        [ ( "geometry", encodeGeometry x.purpleGeometry )
        , ( "properties", encodeProperties x.purpleProperties )
        , ( "type", Jenc.string x.purpleType )
        ]


properties : Jdec.Decoder Properties
properties =
    Jpipe.decode Properties
        |> Jpipe.required "COMMENTS" Jdec.string
        |> Jpipe.required "HUBLOCALITY" Jdec.string
        |> Jpipe.required "LINKTOPHOTO" Jdec.string
        |> Jpipe.required "LOCATION" Jdec.string
        |> Jpipe.required "MANAGED_BY" Jdec.string
        |> Jpipe.required "OBJECTID" Jdec.int
        |> Jpipe.required "PART" Jdec.string
        |> Jpipe.required "SIZE_" Jdec.string
        |> Jpipe.required "STATUS" Jdec.string
        |> Jpipe.required "STREET" Jdec.string
        |> Jpipe.required "TOWN" Jdec.string
        |> Jpipe.required "URN" Jdec.string
        |> Jpipe.required "USED" Jdec.string
        |> Jpipe.required "WARD" Jdec.string


encodeProperties : Properties -> Jenc.Value
encodeProperties x =
    Jenc.object
        [ ( "COMMENTS", Jenc.string x.cOMMENTS )
        , ( "HUBLOCALITY", Jenc.string x.hUBLOCALITY )
        , ( "LINKTOPHOTO", Jenc.string x.lINKTOPHOTO )
        , ( "LOCATION", Jenc.string x.lOCATION )
        , ( "MANAGED_BY", Jenc.string x.mANAGEDBY )
        , ( "OBJECTID", Jenc.int x.oBJECTID )
        , ( "PART", Jenc.string x.pART )
        , ( "SIZE_", Jenc.string x.sIZE )
        , ( "STATUS", Jenc.string x.sTATUS )
        , ( "STREET", Jenc.string x.sTREET )
        , ( "TOWN", Jenc.string x.tOWN )
        , ( "URN", Jenc.string x.uRN )
        , ( "USED", Jenc.string x.uSED )
        , ( "WARD", Jenc.string x.wARD )
        ]


geometry : Jdec.Decoder Geometry
geometry =
    Jpipe.decode Geometry
        |> Jpipe.required "coordinates" (Jdec.list Jdec.float)
        |> Jpipe.required "type" Jdec.string


encodeGeometry : Geometry -> Jenc.Value
encodeGeometry x =
    Jenc.object
        [ ( "coordinates", makeListEncoder Jenc.float x.coordinates )
        , ( "type", Jenc.string x.purpleType )
        ]



--- encoder helpers


makeListEncoder : (a -> Jenc.Value) -> List a -> Jenc.Value
makeListEncoder f arr =
    Jenc.list (List.map f arr)


makeDictEncoder : (a -> Jenc.Value) -> Dict String a -> Jenc.Value
makeDictEncoder f dict =
    Jenc.object (toList (Dict.map (\k -> f) dict))


makeNullableEncoder : (a -> Jenc.Value) -> Maybe a -> Jenc.Value
makeNullableEncoder f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Jenc.null
