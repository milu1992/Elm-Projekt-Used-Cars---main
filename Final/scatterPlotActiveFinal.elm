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

type alias Cars =
    { name : String
    , jahr  : Float
    , kilometerstand :Float
    , pS : Float
    , preisEuro : Float   
    , sitze : Float
    , kilometerPerLiter : Float
    , hubraum : Float
    }

type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Cars -> Float, String)
    | ChangeY (Cars -> Float, String)

type alias Point =
    { pointName : String, x : Float, y : Float }

type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

getData : (Result Http.Error String -> Msg) -> Cmd Msg
getData x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/RicBre/Elm-Projekt-WineInformation/main/Daten/AufbereiteteDaten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    [ "CarCleanFinal.csv.csv"]

csvString_to_data : String -> List Cars
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeCars
        |> Result.toMaybe
        |> Maybe.withDefault []

decodeCars : Csv.Decode.Decoder (Cars -> a) a
decodeCars =
    Csv.Decode.map Cars
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "jahr"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "kilometerstand"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "pS"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "preisEuro"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "sitze"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "kilometerPerLiter"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "hubraum"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )

carsListe :List String -> List Cars
carsListe liste1 =
    List.map(\t -> csvString_to_data t) liste1
        |> List.concat

filterCars : List Cars -> (Cars -> String) -> (Cars -> Float) -> (Cars -> Float) -> String -> String -> XyData
filterCars carsliste a b c x y =
    XyData x y (List.map (\n -> pointName n a b c x y) carsliste)

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

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )

standardExtent : ( number, number1 )
standardExtent =
    ( 0, 100 )

yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
 
addieren : (Float, Float) -> Float-> (Float, Float) 
addieren (min, max) x =
    if min <= 0 then
        ( 0, max + x)
    else 
        (min - x, max + x)
    
wideExtent : List Float -> ( Float, Float )
wideExtent values = 
    let
        result = 
            Maybe.withDefault (0, 0)
            (Statistics.extent values)
        
        max =          
            Maybe.withDefault (0)
            (List.maximum values)
            
        result1 = 
            addieren result (toFloat(tickCount)*max/50)
        
        result2 = 
            addieren result1 (0.0)
        
          
    in
     result2

pointName : Cars -> (Cars -> String) -> (Cars -> Float) -> (Cars -> Float) -> String -> String -> Point
pointName cars u v x y z =
    Point (u cars ++ ", " ++ y ++ ": " ++ String.fromFloat (v cars) ++ "," ++ z ++ ": " ++ String.fromFloat (x cars)) (v cars) (x cars)

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [
            class["point"]
            ,fontSize <| Px 15.0
            ,fontFamily ["serif"]
            ,transform
                [
                    Translate
                    (Scale.convert scaleX xyPoint.x)
                    (Scale.convert scaleY xyPoint.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text xyPoint.pointName]
        ]

scatterplot : XyData -> Svg msg
scatterplot model =
    let

        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35

                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

            
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30

                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

              
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
         
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Laden
    , getData GotText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "Gebrauchtwagen konnten nicht geöffnet werden."

        Loading ->
            Html.text "Gebrauchtwagen werden geöffnet..."

        Success l ->
            let
                cars =
                    filterCars l.data .name l.xAAFunktion l.yAAFunktion l.xName l.yName
            in
            Html.div []
                [
                    ul[][
                        li[][
                            Html.text <| "Suchen eine Eigenschaft für die X-Achse aus"
                            , Html.button [onClick (ChangeX (.jahr, "Baujahr"))][Html.text "Baujahr"]
                            , Html.button [onClick (ChangeX (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                            , Html.button [onClick (ChangeX (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                            , Html.button [onClick (ChangeX (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                            , Html.button [onClick (ChangeX (.sitze, "Sitze"))][Html.text "Sitze"]
                            , Html.button [onClick (ChangeX (.kilometerPerLiter, "Mililiter"))][Html.text "Mililiter"]
                            , Html.button [onClick (ChangeX (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                        ]
                    ]
                    , ul[][
                        li[][
                            Html.text <| "Suchen eine Eigenschaft für die Y-Achse aus"
                            , Html.button [onClick (ChangeY (.jahr, "Baujahr"))][Html.text "Baujahr"]
                            , Html.button [onClick (ChangeY (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                            , Html.button [onClick (ChangeY (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                            , Html.button [onClick (ChangeY (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                            , Html.button [onClick (ChangeY (.sitze, "Sitze"))][Html.text "Sitze"]
                            , Html.button [onClick (ChangeY (.kilometerPerLiter, "Mililiter"))][Html.text "Mililiter"]
                            , Html.button [onClick (ChangeY (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                        ]
                    ] 
                    ,   scatterplot cars
                ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = carsListe [ fullText ], xAAFunktion = .jahr, yAAFunktion = .preisEuro , xName = "Baujahr", yName = "Preis"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunktion = x, yAAFunktion = m.yAAFunktion, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunktion = m.xAAFunktion, yAAFunktion = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )