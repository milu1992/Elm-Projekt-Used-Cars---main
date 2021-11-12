module Main exposing (..)


import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt, toInt)
import Debug exposing (log)

add a b = a + b

type Messages = 
        Add
        | ChangedAddText String
       

init =
    { value = 52, 
    firstName = "Milu",
    numberToAdd = 0 }

view model =
    div [] [
        text (fromInt model.value)
        , div [][]
        , input [ onInput ChangedAddText ][]
        , button [onClick Add][text "Add"]
        ]   

parseUserNumber text = 
    let
        theMaybe = toInt text
    in 
        case theMaybe of 
            Just val ->
                    val
            nothing -> 
                    0 

update msg model = 
    let 
        log1 = log "msg" msg
        log2 = log "model" model
       
    in
    case msg of 
        Add -> 
            {model|value = model.value + model.numberToAdd }
        ChangedAddText theTextThatYouTyped ->        
            {model | numberToAdd = parseUserNumber theTextThatYouTyped }
       

main= 
    Browser.sandbox {
        init=init
        ,   view=view
        ,   update = update
    }