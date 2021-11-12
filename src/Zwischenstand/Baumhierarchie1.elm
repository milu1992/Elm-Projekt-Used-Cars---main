module Zwischenstand.Baumhierarchie1 exposing (..)

import Browser
import Color 
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)