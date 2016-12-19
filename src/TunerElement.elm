module TunerElement exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Css exposing (..)
import Draggable


type alias Model =
    { drag : Draggable.State
    , value : Float
    }


model : Float -> Model
model value =
    { drag = Draggable.init
    , value = value
    }


type Msg
    = DragMsg Draggable.Msg
    | TuneValue Float


dragConfig : Draggable.Config Msg
dragConfig =
    let
        dragDeltaToValueDelta ( _, dy ) =
            -dy / 10
    in
        Draggable.basicConfig <| dragDeltaToValueDelta >> TuneValue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TuneValue delta ->
            { model | value = model.value + delta } ! []

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Draggable.subscriptions DragMsg model.drag


styles =
    Css.asPairs >> Attr.style


view : Model -> Html Msg
view { value } =
    Html.div
        [ styles [ margin (px 64) ]
        ]
        [ Html.label
            [ styles [ marginRight (em 0.5) ] ]
            [ Html.text "Value:" ]
        , Html.div
            [ styles
                [ cursor nsResize
                , borderBottom3 (px 1) dashed (hex "000")
                , width (em 1.5)
                , textAlign center
                , display inlineBlock
                ]
            , Draggable.mouseTrigger "" DragMsg
            ]
            [ Html.text <| toString <| Basics.round value ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( model 0, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
