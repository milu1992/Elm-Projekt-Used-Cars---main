module Zwischenstand.ParaKoodrinaten exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, a, li, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



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
    , ersteFunktion : Cars -> Float
    , zweiteFunktion : Cars -> Float
    , dritteFunktion : Cars -> Float
    , vierteFunktion : Cars -> Float
    , ersterName : String
    , zweiterName : String
    , dritterName : String
    , vierterName : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getData GotText
    )

getData : (Result Http.Error String -> Msg) -> Cmd Msg
getData x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://github.com/milu1992/Elm-Projekt-Used-Cars---main/blob/master/Data/Aufbereitete%20Daten/CarCleanFinal.csv.csv/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    ["CarCleanFinal.csv.csv"]

csvStringToValue : String -> List Cars
csvStringToValue csvRaw =
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
            |> Csv.Decode.andMap (Csv.Decode.field "kraftstoff" ok
            |> Csv.Decode.andMap (Csv.Decode.field "schaltung" ok
            |> Csv.Decode.andMap (Csv.Decode.field "besitzer" ok
            |> Csv.Decode.andMap (Csv.Decode.field "kilometerPerLiter"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "hubraum"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )

--- Update function deklarieren ---
type Msg
    = GotText (Result Http.Error String)
    | Change1 (Cars -> Float, String)
    | Change2 (Cars -> Float, String)
    | Change3 (Cars -> Float, String)
    | Change4 (Cars -> Float, String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = CarsListe [ fullText ], ersteFunktion = .jahr, zweiteFunktion = .kilometerstand, dritteFunktion = .pS, vierteFunktion = .preisEuro , ersterName = "Baujahr", zweiterName = "Kilometerstand", dritterName = "Pferdestärken", vierterName = "PreisinEuro"}, Cmd.none )

                      Err _ ->
                    ( model, Cmd.none )
       Change1 (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = x, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = a, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                     _ ->
                    ( model, Cmd.none )
        Change2 (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = y, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = a, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                ( model, Cmd.none )
        Chamge3 (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName}, Cmd.none )

        _ ->
                    ( model, Cmd.none )
        Change4 (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a}, Cmd.none )








