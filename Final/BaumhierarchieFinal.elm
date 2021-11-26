module Entwicklung.ElmBaumHierarchi exposing (..)

import Browser
import Color
import Html exposing (Html, div, text)
import Http
import Json.Decode
import TreeDiagram
import TreeDiagram.Svg
import TypedSvg exposing (circle, g, line, text_)
import TypedSvg.Attributes exposing (fill, stroke, textAnchor, transform, fontFamily, fontSize)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import TreeDiagram exposing (node, TreeOrientation, topToBottom)

type alias Model =
    { tree : TreeDiagram.Tree String, errorMsg : String }

type Msg
    = GotFlare (Result Http.Error (TreeDiagram.Tree String))

type alias TreeLayout =
    { orientation : TreeOrientation
    , levelHeight : Int
    , subtreeDistance : Int
    , siblingDistance : Int
    , padding : Int
    }

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

drawLine : (Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    line
        [ x1 0, y1 0, x2 targetX, y2 targetY, stroke (ST.Paint Color.darkGrey) ]
        []

drawNode : String -> Svg msg
drawNode n =
    g
        []
        [ circle 
            [ r 16
            , stroke (Paint Color.red)
            , fill (Paint Color.red)
            , cx 0
            , cy 0 
            ] 
            []
        , text_ 
            [ textAnchor AnchorEnd
            , transform 
                [ Translate -5.5 -20.5 
                , Rotate 60.0 0.0 0.0
                ]
            , fontFamily [ "calibri" ]
            , fontSize (Px 12)
            ] 
            [ text n ]
        ]

newTreeLayout =
    TreeLayout topToBottom 250 200 30 100

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        } 

init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = TreeDiagram.node "" [], errorMsg = "Loading ..." }
    , Http.get { url = "https://raw.githubusercontent.com/milu1992/Elm-Projekt-Used-Cars---main/master/Data/Aufbereitete%20Daten/LocationCar.json", expect = Http.expectJson GotFlare treeDecoder }
    )
view : Model -> Html Msg
view model =
    div []
        [ TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout drawNode drawLine model.tree 

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




