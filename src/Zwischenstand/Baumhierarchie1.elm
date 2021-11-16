module Zwischenstand.Baumhierarchie1 exposing (..)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))

--- Start Baumdiagramm 
type alias Model =
    { tree : TreeDiagram.Tree String, errorMsg : String }

init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = TreeDiagram.node "" [], errorMsg = "Loading ..." }
    , Http.get { url = "http://github.com/milu1992/Elm-Projekt-Used-Cars---main/blob/master/Data/LocationCar.json", expect = Http.expectJson GotFlare treeDecoder }
    )

treeDecoder : Json.Decode.Decoder (TreeDiagram.Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    TreeDiagram.node name []

                Just c ->
                    TreeDiagram.node name c
        )
        (Json.Decode.field "data" (Json.Decode.field "id" Json.Decode.string))
                (Json.Decode.maybe <|
                    Json.Decode.field "children" <|
                        Json.Decode.list <|
                            Json.Decode.lazy
                                (\_ -> treeDecoder)
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = TreeDiagram.node "" []
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )


drawLine : (Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 0, y1 0, x2 targetX, y2 targetY, stroke (ST.Paint Color.black) ]
        []

