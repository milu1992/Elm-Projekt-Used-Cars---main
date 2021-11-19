module Zwischenstand.Scatter exposing (..)

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
  | Success (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/milu1992/Elm-Projekt-Used-Cars---main/master/Data/Quelldaten/train.csv?token=AWFPZWZWDL3236OQAFZV4FDBSY7K4/" ++ datensatz
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch
    )

liste : List String
liste =
    [ "CarCleanFinal.csv"]

csvString_to_data : String -> List (String, Maybe Float, Maybe Float)
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []

decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\a b c-> ( a, Just b, Just c ))
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "preisEuro" 
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap
                        (Csv.Decode.field "jahr" 
                            (String.toFloat >> Result.fromMaybe "error parsing string")
                                
                        )
                )
        )


transform : List (String, Maybe Float, Maybe Float) -> List (String, String, String)
transform ganzerText =
    List.map (\( a, b, c ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden", c |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden")) ganzerText

transform2 : List (String, Maybe Float, Maybe Float) -> List (String, Float, Float)
transform2 ganzerText =
    List.map(\(a, b, c) -> (a, b |> Maybe.withDefault 0.0, c |> Maybe.withDefault 0.0)) ganzerText

type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentList =
            case model of
                Success l ->
                    l

                Failure ->
                    []

                Loading ->
                    []
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| currentList ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5

