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

