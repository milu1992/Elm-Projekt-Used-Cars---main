module Zwischenstand.Baumhierarchie1 exposing (..)

import Browser
import Color 
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)