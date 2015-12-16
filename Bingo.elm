module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp
import Signal exposing (Address)

import BingoUtils as Utils

-- MODEL
type alias Entry = 
    { 
        phrase: String,
        points: Int,
        wasSpoken: Bool,
        id: Int
    }

type alias Model = 
    { 
        entries: List Entry,
        phraseInput: String,
        pointsInput: String,
        nextID: Int
    }

intialModel : Model
intialModel =
    {
        entries = 
            [ ],
        phraseInput = "",
        pointsInput = "",
        nextID = 0
    }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
    Entry phrase points False id

-- UPDATE

type Action = 
    NoOp 
    | Sort
    | Delete Int
    | Mark Int
    | UpdatePhraseInput String
    | UpdatePointsInput String
    | Add
 
update : Action -> Model -> Model
update action model = 
    case action of 
        NoOp -> 
            model

        Sort -> 
            { model | entries = List.sortBy .points model.entries }

        Delete id ->
            let 
                remainingEntries = 
                    List.filter (\e -> e.id /= id) model.entries
            in 
                {  model | entries =  remainingEntries}

        Mark id ->
            let
                updateEntry e =
                    if e.id == id then { e | wasSpoken = (not e.wasSpoken)} else e
            in
                { model | entries = List.map updateEntry model.entries }

        UpdatePhraseInput content ->
            {model | phraseInput = content}

        UpdatePointsInput points ->
            { model | pointsInput = points}

        Add ->
            let
                entryToAdd =
                    newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextID
                isInvalid model =
                    String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
            in
                if isInvalid model
                then model
                else
                    { model |
                            phraseInput = "",
                            pointsInput = "",
                            entries = entryToAdd :: model.entries,
                            nextID = model.nextID + 1
                    }


-- VIEW 
title : String -> Int -> Html
title message times = 
    message ++ " "
        |> toUpper 
        |> repeat times 
        |> trimRight
        |> text

totalPoints : List Entry -> Int
totalPoints entries =
    entries
    |> List.filter .wasSpoken
    |> List.map .points
    |> List.foldl (+) 0


totalItem : Int -> Html
totalItem total =
    li 
        [ class "total" ]
        [
            span [class "label"] [text "Total"],
            span [class "points"] [text (toString total)]
        ]

entryItem : Address Action -> Entry -> Html
entryItem address entry =
    li 
    [ 
        classList [ ("highlight", entry.wasSpoken) ],
        onClick address (Mark entry.id)
    ]
    [
        span [ class "phrase"] [ text entry.phrase],
        span [ class "points "] [ text (toString entry.points) ],
        button 
            [ class "delete", onClick address (Delete entry.id)]
            [ ]
    ]

entryList : Address Action -> List Entry -> Html
entryList address entries = 
    let
        entryItems = List.map (entryItem address) entries
        items = entryItems ++ [ totalItem (totalPoints entries )]
    in
        ul [] items


pageHeader : Html
pageHeader =
    h1 [ id "logo", class "classy"] [ title "Bingo" 3 ]

pageFooter : Html
pageFooter =
    footer [  ]
        [ 
            a [ href "http://kristengarnier.com"] [ text "kristen garnier's website"]
        ]


entryForm : Address Action -> Model -> Html
entryForm adress model = 
    div []
        [
            input 
                [ type' "text",
                 placeholder "Phrase", 
                 value model.phraseInput, 
                 name "phrase",
                 autofocus True,
                 Utils.onInput adress UpdatePhraseInput]
                [ ],
            input
                [ type' "number",
                 placeholder "Points",
                 value model.pointsInput,
                 name "points",
                 Utils.onInput adress UpdatePointsInput]
                 [ ],
            button [class "add", onClick adress Add] [text "Add"],
            h2
                [ ]
                [ text (model.phraseInput ++ " " ++ model.pointsInput) ]

        ]


view : Address Action -> Model -> Html
view address model = 
    div [ id "container"] [
        pageHeader,
        entryForm address model,
        entryList address model.entries,
        button 
            [class "sort", onClick address Sort ] 
            [text "sort"],
        pageFooter
    ]


-- RENDERING
main : Signal Html
main =
    StartApp.start 
        {
            model = intialModel,
            view = view,
            update = update
        }

    
