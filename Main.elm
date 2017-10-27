module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import String
import String.Format exposing (..)
import Material
import Material.Scheme
import Material.Grid as Grid
import Material.List as Lists
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options as Options exposing (css)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Roll =
    { roll : List Int
    , total : Int
    , num : Int
    , sides : Int
    }


type alias Model =
    { rolls : List Roll
    , num : String
    , sides : String
    , mdl : Material.Model
    , error : Maybe String
    }


model : Model
model =
    Model [] "1" "6" Material.model Nothing



-- UPDATE


type Msg
    = NewNum String
    | NewSides String
    | RollDice
    | NewResult Roll
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewNum n ->
            case String.toInt n of
                Ok _ ->
                    ( { model | num = n, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | num = n, error = Just error }, Cmd.none )

        NewSides s ->
            case String.toInt s of
                Ok _ ->
                    ( { model | sides = s, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | sides = s, error = Just error }, Cmd.none )

        RollDice ->
            ( model
            , case (rollDice model.num model.sides) of
                Just gen ->
                    Random.generate NewResult gen

                Nothing ->
                    Cmd.none
            )

        NewResult roll ->
            ( { model | rolls = roll :: model.rolls }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


rollDice : String -> String -> Maybe (Random.Generator Roll)
rollDice num sides =
    case ( String.toInt num, String.toInt sides ) of
        ( Ok num, Ok sides ) ->
            let
                roll =
                    Random.list num (Random.int 1 sides)

                gen =
                    Random.map (\l -> Roll l (List.foldl (+) 0 l) num sides) roll
            in
                Just gen

        _ ->
            Nothing



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
            model.mdl
            [ Layout.fixedHeader ]
            { header = [ h3 [ style [ ( "padding", "2rem" ) ] ] [ text "Die Roller" ] ]
            , drawer = []
            , tabs = ( [], [] )
            , main = [ viewBody model ]
            }


viewBody : Model -> Html Msg
viewBody model =
    div [ style [ ( "padding", "2rem" ) ] ]
        [ Grid.grid []
            [ Grid.cell [ Grid.size Grid.All 4 ]
                [ numberField model 0 NewNum "Num" model.num
                , numberField model 1 NewSides "Sides" model.sides
                , Button.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Button.raised
                    , Button.colored
                    , Button.ripple
                    , Options.onClick RollDice
                    , Button.disabled |> Options.when (not (model.error == Nothing))
                    ]
                    [ Icon.i "casino"
                    , text "  Roll Dice"
                    ]
                ]
            , Grid.cell [ Grid.size Grid.All 6 ]
                [ Lists.ul []
                    (List.map
                        (\r ->
                            Lists.li []
                                [ Lists.content []
                                    [ text (format3 "{1}d{2}: {3}" ( r.num, r.sides, r.roll )) ]
                                , Lists.content2 []
                                    [ text (toString r.total) ]
                                ]
                        )
                        model.rolls
                    )
                ]
            ]
        ]
        |> Material.Scheme.top


numberField : Model -> Int -> (String -> Msg) -> String -> String -> Html Msg
numberField model idNum msg lbl val =
    Textfield.render Mdl
        [ idNum ]
        model.mdl
        [ Options.onInput msg
        , Textfield.label lbl
        , Textfield.floatingLabel
        , Textfield.text_
        , Textfield.value val
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )
