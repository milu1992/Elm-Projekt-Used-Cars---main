module Zwischenstand.ScatterActive exposing (..)

import Axis
import Html exposing (Html,text, pre)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import TypedSvg.Attributes exposing (name)
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (onClick)
import Zwischenstand.ParaKoodrinaten exposing (cars)

main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Model
  = Failure
  | Loading
  | Success 
    { data : List Cars
    , xAAFunktion : Cars -> Float
    , yAAFunktion : Cars -> Float
    , xName : String
    , yName : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://github.com/milu1992/Elm-Projekt-Used-Cars---main/tree/master/Data/Aufbereitete%20Daten" ++ datensatz
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch
    )

liste : List String
liste =
    [ "CarCleanFinal.csv.csv"]

csvString_to_data : String -> List Cars
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCars
        |> Result.toMaybe
        |> Maybe.withDefault []

type alias Cars =
    { name : String
    , jahr  : Float
    , kilometerstand :Float
    , pS : Float
    , preisEuro : Float   
    , sitze : Float
    , kraftstoff : String
    , schaltung : String
    , besitzer : String
    , kilometerPerLiter : Float
    , hubraum : Float
    
    
    }

decodeCars : Csv.Decode.Decoder (Cars -> a) a
decodeCars =
    Csv.Decode.map Cars
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "jahr"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "kilometerstand"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "pS"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "preisEuro"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "sitze"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "kraftstoff" ok)
            |> Csv.Decode.andMap (Csv.Decode.field "schaltung" ok)
            |> Csv.Decode.andMap (Csv.Decode.field "besitzer" ok)
            |> Csv.Decode.andMap (Csv.Decode.field "kilometerPerLiter"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "hubraum"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )
type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Cars -> Float, String)
    | ChangeY (Cars -> Float, String)
