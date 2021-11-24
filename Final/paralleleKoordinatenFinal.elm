module Entwicklung.ElmParalleleKoordinaten exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, a, li, ul)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox, class)
import TypedSvg.Attributes.InPx exposing (x, y)
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
    | Change1 (Cars -> Float, String)
    | Change2 (Cars -> Float, String)
    | Change3 (Cars -> Float, String)
    | Change4 (Cars -> Float, String)

type alias MultiDimPoint =
    { pointName : String, value : List Float }

type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

getData : (Result Http.Error String -> Msg) -> Cmd Msg
getData x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/milu1992/Elm-Projekt-Used-Cars---main/master/Data/Quelldaten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    ["Final.csv"]

csvStringToValue : String -> List Cars
csvStringToValue csvRaw =
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
    List.map(\t -> csvStringToValue t) liste1
        |> List.concat

padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount_ : Int
tickCount_ =
    8


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount_)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )

parallelCoodinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoodinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount_ ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            [
                TypedSvg.Core.text """
                .parallelerPunkt { stroke: rgba(1, 0, 0,0.2);}
                .parallelerPunkt:hover {stroke: rgb(173, 255, 47); stroke-width: 2;} 
                .parallelerPunkt text { display: none; }
                .parallelerPunkt:hover text { display: inline; stroke: rgb(0, 0, 0); stroke-width: 0.1; font-size: small; font-family: calibri}  
                """
            ]
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p name beschreibung =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in 
                        g [class ["parallelerPunkt"]][
                            Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            , class ["parallelerPunkt"]
                            ]
                            , text_
                                [ x 300
                                , y -20
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [ TypedSvg.Core.text (name++ (String.concat<|(List.map2(\a b-> ", " ++b++ ": "++ (String.fromFloat a))p beschreibung)))]
                                
                        ]

                    in
                    model.data
                        |> List.map
                            (\dataset ->
                                g [ transform [ Translate (padding - 1) padding ] ]
                                    (List.map (\a -> drawPoint a.value a.pointName model.dimDescription) dataset)
                            )
                    )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , getData GotText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model = 
    case model of
        Failure ->
            Html.text "Gebrauchtwagen konnten leider nicht geöffnet werden."

        Loading ->
            Html.text "Gebrauchtwagen werden geladen..."

        Success l ->
                    let
                        multiDimDaten : List Cars -> (Cars -> Float) -> (Cars -> Float) -> (Cars -> Float) -> (Cars -> Float) -> (Cars -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeCars a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (e x)
                                )
                                listeCars
                            ]

                        plotDaten = 
                            multiDimDaten l.data l.ersteFunktion l.zweiteFunktion l.dritteFunktion l.vierteFunktion .name l.ersterName l.zweiterName l.dritterName l.vierterName  
                    in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die erste Spalte aus"
                                    , Html.button [onClick (Change1 (.jahr, "Baujahr"))][Html.text "Baujahr"]
                                    , Html.button [onClick (Change1 (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                                    , Html.button [onClick (Change1 (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                                    , Html.button [onClick (Change1 (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                                    , Html.button [onClick (Change1 (.sitze, "Sitze"))][Html.text "Sitze"]
                                    , Html.button [onClick (Change1 (.kilometerPerLiter, "KilometerPerLiterr"))][Html.text "kilometerPerLiter"]
                                    , Html.button [onClick (Change1 (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                                ]                           
                            ]
                            , ul[][
                                li[][
                                  Html.text <| "Suchen eine Eigenschaft für die zweite Spalte aus"
                                    , Html.button [onClick (Change2 (.jahr, "Baujahr"))][Html.text "Baujahr"]
                                    , Html.button [onClick (Change2 (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                                    , Html.button [onClick (Change2 (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                                    , Html.button [onClick (Change2 (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                                    , Html.button [onClick (Change2 (.sitze, "Sitze"))][Html.text "Sitze"]
                                    , Html.button [onClick (Change2 (.kilometerPerLiter, "kilometerPerLiter"))][Html.text "kilometerPerLiter"]
                                    , Html.button [onClick (Change2 (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                                ]    
                            ] 
                            , ul[][
                                li[][
                                  Html.text <| "Suchen eine Eigenschaft für die dritte Spalte aus"
                                    , Html.button [onClick (Change3 (.jahr, "Baujahr"))][Html.text "Baujahr"]
                                    , Html.button [onClick (Change3 (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                                    , Html.button [onClick (Change3 (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                                    , Html.button [onClick (Change3 (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                                    , Html.button [onClick (Change3 (.sitze, "Sitze"))][Html.text "Sitze"]
                                    , Html.button [onClick (Change3 (.kilometerPerLiter, "kilometerPerLiter"))][Html.text "kilometerPerLiter"]
                                    , Html.button [onClick (Change3 (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                                ]    
                            ] 
                            , ul[][
                                li[][
                                  Html.text <| "Suchen eine Eigenschaft für die vierte Spalte aus"
                                    , Html.button [onClick (Change4 (.jahr, "Baujahr"))][Html.text "Baujahr"]
                                    , Html.button [onClick (Change4 (.kilometerstand, "Kilometerstand"))][Html.text "Kilometerstand"]
                                    , Html.button [onClick (Change4 (.pS, "Pferdestärken"))][Html.text "Pferdestärken"]
                                    , Html.button [onClick (Change4 (.preisEuro, "PreisInEuro"))][Html.text "PreisInEuro"]
                                    , Html.button [onClick (Change4 (.sitze, "Sitze"))][Html.text "Sitze"]
                                    , Html.button [onClick (Change4 (.kilometerPerLiter, "kilometerPerLiter"))][Html.text "kilometerPerLiter"]
                                    , Html.button [onClick (Change4 (.hubraum, "Hubraum"))][Html.text "Hubraum"]
                                ]    
                            ] 
                                ,parallelCoodinatesPlot 600 2 plotDaten
                        ]

update : Msg -> Model -> ( Model, Cmd Msg ) 
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = carsListe [ fullText ], ersteFunktion = .jahr, zweiteFunktion = .kilometerstand, dritteFunktion = .pS, vierteFunktion = .preisEuro , ersterName = "Baujahr", zweiterName = "Kilometerstand", dritterName = "Pferdestärken", vierterName = "PreisinEuro"}, Cmd.none )

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
        Change3 (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Change4 (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )










