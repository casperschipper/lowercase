module Main exposing (..)

import Browser
import Html exposing (Html, br, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { input : String
    , output : String
    }


init : Model
init =
    let
        preset =
            "some titles are better than others"
    in
    { input = preset
    , output = title preset
    }


type Msg
    = ChangedTitle String


update : Msg -> b -> { output : String, input : String }
update msg model =
    case msg of
        ChangedTitle str ->
            { output = title str
            , input = str
            }


lowerWords : List String
lowerWords =
    [ "the"
    , "in"
    , "of"
    , "an"
    , "a"
    , "or"
    , "and"
    , "under"
    , "is"
    , "at"
    , "are"
    , "be"
    , "not"
    , "than"
    , "by"
    , "under"
    , "on"
    ]


capitalizeIf : String -> String
capitalizeIf input =
    let
        lower =
            String.toLower input
    in
    if List.member lower lowerWords then
        lower

    else
        input
            |> String.uncons
            |> Maybe.map
                (\( first, rest ) ->
                    (first |> Char.toUpper |> String.fromChar) ++ String.toLower rest
                )
            |> Maybe.withDefault ""


title : String -> String
title input =
    input
        |> String.split " "
        |> List.map capitalizeIf
        |> String.join " "


view : Model -> Html Msg
view model =
    div []
        [ Html.p [] [ text "this capitalizes titles in the correct way" ]
        , input [ style "width" "100%", onInput ChangedTitle, placeholder "put title here", value model.input ] []
        , br [] []
        , input [ style "width" "100%", value model.output ] []
        ]
