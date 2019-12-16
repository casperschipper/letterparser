module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict
import Html exposing (Html, br, button, div, input, label, p, text, textarea)
import Html.Attributes exposing (class, cols, rows, style)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { input : String
    , symbolDict : Dict.Dict String String
    , result : String
    , program : List String
    }


init : Model
init =
    let
        m =
            { input = ""
            , symbolDict = Dict.empty
            , result = ""
            , program = []
            }
    in
    update (UpdateProgram "a -> aba\nb->bbabbc\nc->cccca") m



-- UPDATE


type Msg
    = InputText String
    | UpdateSymbol Symbol String
    | Parse
    | RunProgram
    | UpdateProgram String


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
    | SPACE
    | ILLEGAL


type Statement
    = Statement String String -- from to


fromString : String -> Maybe Statement
fromString input =
    let
        onlyTwo : List a -> Maybe (List a)
        onlyTwo xs =
            case xs of
                [ x, y ] ->
                    Just [ x, y ]

                _ ->
                    Nothing

        split1 =
            onlyTwo << String.split "->"

        split2 =
            onlyTwo << String.split ">"

        killHeadSpace : String -> String
        killHeadSpace str =
            case String.uncons str of
                Just ( ' ', rest ) ->
                    killHeadSpace rest

                Just _ ->
                    str

                Nothing ->
                    str

        killSpace : String -> String
        killSpace =
            String.reverse << killHeadSpace << String.reverse << killHeadSpace
    in
    case oneOf split1 split2 input of
        Just [ from, to ] ->
            let
                _ =
                    Debug.log "from -> to" ( from, to )
            in
            Just <|
                Statement (killSpace from) (killSpace to)

        _ ->
            Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputText string ->
            { model | input = string }

        UpdateSymbol symbol string ->
            { model | symbolDict = Dict.insert (asString symbol) string model.symbolDict }

        Parse ->
            { model | result = eval model.symbolDict model.input }

        UpdateProgram string ->
            let
                split =
                    String.split "\n"
            in
            { model | program = split string }

        RunProgram ->
            case model.program of
                [] ->
                    model

                -- foldr : (a -> b -> b) -> b -> List a -> b
                -- foldr :
                program ->
                    let
                        statements =
                            List.filterMap fromString program

                        oneFold : Statement -> String -> String
                        oneFold statement current =
                            let
                                ( from, to ) =
                                    case statement of
                                        Statement f t ->
                                            ( f, t )

                                -- _ =
                                --     Debug.log "f,t" <| String.replace from to current
                            in
                            String.replace from to current

                        result =
                            List.foldr oneFold model.input (List.reverse statements)
                    in
                    { model | input = result }


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

        ' ' ->
            SPACE

        _ ->
            ILLEGAL


symbolFromString : String -> Symbol
symbolFromString str =
    case str of
        "a" ->
            A

        "b" ->
            B

        "c" ->
            C

        "d" ->
            D

        "e" ->
            E

        "f" ->
            F

        "g" ->
            G

        "h" ->
            H

        "i" ->
            I

        " " ->
            SPACE

        _ ->
            ILLEGAL


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


eval : Dict.Dict String String -> String -> String
eval fields input =
    let
        keys : List Symbol
        keys =
            List.map symbolFromString <| Dict.keys fields

        getValue : Symbol -> String
        getValue symbol =
            let
                try =
                    Dict.get (asString symbol) fields
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


oneOf : (String -> Maybe a) -> (String -> Maybe a) -> String -> Maybe a
oneOf parse1 parse2 input =
    case parse1 input of
        Nothing ->
            case parse2 input of
                Nothing ->
                    Nothing

                result ->
                    result

        something ->
            something



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
    , SPACE
    , ILLEGAL
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
            "d"

        E ->
            "e"

        F ->
            "f"

        G ->
            "g"

        H ->
            "h"

        I ->
            "i"

        SPACE ->
            "space"

        ILLEGAL ->
            "illegal"


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
        [ label []
            [ text "program"
            , textarea
                [ onInput UpdateProgram
                , rows 10
                , cols 100
                , style "display" "block"
                ]
                []
            , button [ onClick RunProgram ] [ text "run program" ]
            ]
        , br [] []
        , label []
            [ text "Current / Startstring"
            , textarea
                [ onInput InputText
                , style "display" "block"
                , rows 10
                , cols 100
                , Html.Attributes.value model.input
                ]
                []
            , p [] [ text <| String.fromInt <| String.length model.input ]
            ]
        , div [ class "symbols" ] <| List.map makeSymbol allSymbols
        , button [ onClick Parse ] [ text "parse" ]
        , p [] [ text model.result ]
        ]
