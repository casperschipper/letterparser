module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict
import Html exposing (Html, br, button, div, input, label, p, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , symbolDict : Dict.Dict Char String
    , result : String
    }


init : Model
init =
    { input = ""
    , symbolDict = Dict.empty
    , result = ""
    }



-- UPDATE


type Msg
    = InputText String
    | UpdateSymbol Symbol String
    | Parse


type Symbol
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputText string ->
            { model | input = string }

        UpdateSymbol symbol string ->
            { model | symbolDict = Dict.insert (asChar symbol) string model.symbolDict }

        Parse ->
            { model | result = eval model.symbolDict model.input }


fromChar : Char -> Symbol
fromChar char =
    case char of
        'a' ->
            A

        'b' ->
            B

        'c' ->
            C

        'd' ->
            D

        'e' ->
            E

        'f' ->
            F

        'g' ->
            G

        'h' ->
            H

        'i' ->
            I

        _ ->
            A


firstChar : String -> Char
firstChar str =
    case String.uncons str of
        Just ( s, _ ) ->
            s

        Nothing ->
            ' '


asChar : Symbol -> Char
asChar =
    firstChar << asString


eval : Dict.Dict Char String -> String -> String
eval fields input =
    let
        keys : List Symbol
        keys =
            List.map fromChar <| Dict.keys fields

        getValue : Symbol -> String
        getValue symbol =
            let
                try =
                    Dict.get (asChar symbol) fields
            in
            case try of
                Nothing ->
                    ""

                Just value ->
                    value

        lexed : String -> List Symbol
        lexed =
            List.map fromChar << String.toList

        strings : List Symbol -> List String
        strings =
            List.map getValue

        pipe : String -> List String
        pipe =
            strings << lexed
    in
    String.concat <| pipe input



-- VIEW


allSymbols : List Symbol
allSymbols =
    [ A
    , B
    , C
    , D
    , E
    , F
    , G
    , H
    , I
    ]


asString : Symbol -> String
asString field =
    case field of
        A ->
            "a"

        B ->
            "b"

        C ->
            "c"

        D ->
            "e"

        E ->
            "f"

        F ->
            "f"

        G ->
            "h"

        H ->
            "i"

        I ->
            "j"


view : Model -> Html Msg
view model =
    let
        makeSymbol : Symbol -> Html Msg
        makeSymbol field =
            label []
                [ text <| "symbol " ++ asString field
                , input [ onInput <| UpdateSymbol field ] []
                , br [] []
                ]
    in
    div []
        [ textarea
            [ onInput InputText
            , cols 30
            , rows 20
            ]
            []
        , div [ class "symbols" ] <| List.map makeSymbol allSymbols
        , button [ onClick Parse ] [ text "parse" ]
        , p [] [ text model.result ]
        ]
