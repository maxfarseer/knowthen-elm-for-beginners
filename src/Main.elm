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


add : Model -> Model
add model =
    let
        player =
            { id = List.length model.players, name = model.name, points = 0 }

        newPlayers =
            player :: model.players
    in
    { model
        | players = newPlayers
        , name = ""
    }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }

                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }

                    else
                        play
                )
                model.plays
    in
    { model
        | players = newPlayers
        , plays = newPlays
        , name = ""
        , playerId = Nothing
    }


updatePlayerPoints : Model -> Player -> Int -> Model
updatePlayerPoints model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }

                    else
                        player
                )
                model.players

        newPlaysRecord =
            { id = List.length model.plays
            , playerId = scorer.id
            , name = scorer.name
            , points = points
            }
    in
    { model | players = newPlayers, plays = newPlaysRecord :: model.plays }


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            List.filter
                (\p -> p.id /= play.id)
                model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - play.points }

                    else
                        player
                )
                model.players
    in
    { model | players = newPlayers, plays = newPlays }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input name ->
            ( { model | name = name }, Cmd.none )

        Save ->
            if String.isEmpty model.name then
                ( model, Cmd.none )

            else
                ( save model, Cmd.none )

        Edit player ->
            ( { model | name = player.name, playerId = Just player.id }, Cmd.none )

        Score player points ->
            ( updatePlayerPoints model player points, Cmd.none )

        DeletePlay play ->
            ( deletePlay model play, Cmd.none )

        Cancel ->
            ( { model | name = "", playerId = Nothing }, Cmd.none )


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
        , playerSection model
        , playerForm model
        , playSection model
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
            [ Html.text "Save" ]
        , Html.button
            [ Attr.type_ "button"
            , Events.onClick Cancel
            ]
            [ Html.text "Cancel" ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    Html.div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    Html.header []
        [ Html.div [] [ Html.text "Name" ]
        , Html.div [] [ Html.text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map playerEntry
        |> Html.ul []


playerEntry : Player -> Html Msg
playerEntry player =
    Html.li []
        [ Html.i
            [ Attr.class "edit"
            , Events.onClick (Edit player)
            ]
            [ Html.text "[EDIT]" ]
        , Html.div []
            [ Html.text player.name ]
        , Html.button
            [ Attr.type_ "button"
            , Events.onClick (Score player 2)
            ]
            [ Html.text "2pt" ]
        , Html.button
            [ Attr.type_ "button"
            , Events.onClick (Score player 3)
            ]
            [ Html.text "3pt" ]
        , Html.div []
            [ Html.text (String.fromInt player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        totalPoints =
            List.map .points model.players |> List.sum
    in
    Html.div
        []
        [ Html.div [] [ Html.text "Total points" ]
        , Html.div [] [ Html.text <| String.fromInt totalPoints ]
        ]


playSectionHeader : Html Msg
playSectionHeader =
    Html.header
        []
        [ Html.div [] [ Html.text "Plays" ]
        , Html.div [] [ Html.text "Points" ]
        ]


playSectionEntry : Play -> Html Msg
playSectionEntry play =
    Html.li
        []
        [ Html.i
            [ Attr.class "edit"
            , Events.onClick (DeletePlay play)
            ]
            [ Html.text "[DEL] " ]
        , Html.div [] [ Html.text play.name ]
        , Html.div [] [ Html.text <| String.fromInt play.points ]
        ]


playSectionList : Model -> Html Msg
playSectionList model =
    model.plays
        |> List.map playSectionEntry
        |> Html.ul []


playSection : Model -> Html Msg
playSection model =
    Html.div
        []
        [ playSectionHeader
        , playSectionList model
        ]
