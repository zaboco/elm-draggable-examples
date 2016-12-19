module TunerElement exposing (..)

import Draggable.Events exposing (onDragBy, onDragEnd, onDragStart)
import Html exposing (Html)
import Html.Attributes as Attr
import Css exposing (..)
import Draggable
import Keyboard.Key as Key
import Keyboard exposing (KeyCode)


type Tunable
    = Fixed Float
    | Tuning { initial : Float, major : Float, minor : Float }


type TuneMode
    = Minor
    | Major


roundFloat : Float -> Float
roundFloat =
    Basics.round >> toFloat


evalTunable : Tunable -> Float
evalTunable tunable =
    case tunable of
        Fixed value ->
            value

        Tuning { initial, major, minor } ->
            initial + roundFloat major * 10 + roundFloat minor


increaseTunable : TuneMode -> Float -> Tunable -> Tunable
increaseTunable mode delta tunable =
    case ( tunable, mode ) of
        ( Fixed _, _ ) ->
            tunable

        ( Tuning tuning, Minor ) ->
            Tuning { tuning | minor = tuning.minor + delta }

        ( Tuning tuning, Major ) ->
            Tuning { tuning | major = tuning.major + delta }


toTuning : Tunable -> Tunable
toTuning tunable =
    case tunable of
        Tuning _ ->
            tunable

        Fixed value ->
            Tuning { initial = value, minor = 0, major = 0 }


toFixed : Tunable -> Tunable
toFixed =
    Fixed << evalTunable


type alias Model =
    { drag : Draggable.State
    , value : Tunable
    , mode : TuneMode
    }


model : Float -> Model
model value =
    { drag = Draggable.init
    , value = Fixed value
    , mode = Minor
    }


type Msg
    = DragMsg Draggable.Msg
    | TuneValue Float
    | SetMode TuneMode
    | StartTuning
    | StopTuning
    | NoOp


dragConfig : Draggable.Config Msg
dragConfig =
    let
        dragDeltaToValueDelta ( _, dy ) =
            -dy / 10
    in
        Draggable.customConfig
            [ onDragBy <| dragDeltaToValueDelta >> TuneValue
            , onDragStart <| \_ -> StartTuning
            , onDragEnd StopTuning
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.value, msg ) of
        ( Fixed _, StartTuning ) ->
            { model | value = toTuning model.value } ! []

        ( Tuning _, TuneValue delta ) ->
            { model | value = increaseTunable model.mode delta model.value } ! []

        ( Tuning _, StopTuning ) ->
            { model | value = toFixed model.value } ! []

        ( _, DragMsg dragMsg ) ->
            Draggable.update dragConfig dragMsg model

        ( _, SetMode mode ) ->
            { model | mode = mode } ! []

        _ ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Draggable.subscriptions DragMsg model.drag
        , Keyboard.downs (handleKey True)
        , Keyboard.ups (handleKey False)
        ]


handleKey : Bool -> KeyCode -> Msg
handleKey pressed code =
    case ( pressed, Key.fromCode code ) of
        ( True, Key.Shift _ ) ->
            SetMode Major

        ( False, Key.Shift _ ) ->
            SetMode Minor

        _ ->
            NoOp


styles =
    Css.asPairs >> Attr.style


view : Model -> Html Msg
view { value, mode } =
    let
        borderStyle =
            case ( value, mode ) of
                ( Fixed _, _ ) ->
                    border2 zero none

                ( Tuning _, Minor ) ->
                    borderBottom3 (px 1) dashed (hex "000")

                ( Tuning _, Major ) ->
                    borderBottom3 (px 1) solid (hex "000")
    in
        Html.div
            [ styles [ margin (px 64) ]
            ]
            [ Html.label
                [ styles [ marginRight (em 0.5) ] ]
                [ Html.text "Value:" ]
            , Html.div
                [ styles
                    [ cursor nsResize
                    , borderStyle
                    , width (em 1.5)
                    , textAlign center
                    , display inlineBlock
                    ]
                , Draggable.mouseTrigger "" DragMsg
                ]
                [ Html.text <| toString <| evalTunable value ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model 0, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
