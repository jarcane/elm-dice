module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import String
import Material
import Material.Scheme
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


type alias Model =
    { roll : List Int
    , num : String
    , sides : String
    , mdl : Material.Model
    , error : Maybe String
    }


type Msg
    = NewNum String
    | NewSides String
    | RollDice
    | NewResult (List Int)
    | Mdl (Material.Msg Msg)



-- UPDATE


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
            ( model, Random.generate NewResult (rollDice model.num model.sides) )

        NewResult dice ->
            ( { model | roll = dice }, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


rollDice : String -> String -> Random.Generator (List Int)
rollDice num sides =
    case (String.toInt num, String.toInt sides) of
        (Ok num, Ok sides) ->
            Random.list num (Random.int 1 sides)
        _ ->
            Random.list 1 (Random.int 1 6)  
    


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
            , main = [ viewBody model ]}


viewBody : Model -> Html Msg
viewBody model =
    div [ style [("padding", "2rem")] ]
        [ div  []
            [ numberField model 0 NewNum "Num" model.num
            , numberField model 1 NewSides "Sides" model.sides
            , Button.render Mdl [2] model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick RollDice
                , Button.disabled |> Options.when (not (model.error == Nothing))
                ]
                [ text "Roll Dice"]
            ]
        , div []
            [
                text ("Roll: " ++ toString model.roll ++ "  Total: " ++ toString (List.foldl (+) 0 model.roll))
            ]
        ]
        |> Material.Scheme.top


numberField : Model -> Int -> (String -> Msg) -> String -> String -> Html Msg
numberField model idNum msg lbl val =
    Textfield.render Mdl [idNum] model.mdl
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
    ( Model [] "1" "6" Material.model Nothing, Cmd.none )
