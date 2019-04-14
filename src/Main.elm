module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input name ->
            ( { model | name = name }, Cmd.none )

        Cancel ->
            ( { model | name = "" }, Cmd.none )

        _ ->
            ( initModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "scoreboard" ]
        [ Html.h1 []
            [ Html.text "Score Keeper" ]
        , playerForm model
        , Html.p
            []
            [ Html.text <| Debug.toString model ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ Events.onSubmit Save ]
        [ Html.input
            [ Attr.type_ "text"
            , Attr.placeholder "Add/Edit player..."
            , Events.onInput Input
            , Attr.value model.name
            ]
            []
        , Html.button [ Attr.type_ "submit" ]
            [ Html.text "Add" ]
        , Html.button
            [ Attr.type_ "button"
            , Events.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]
