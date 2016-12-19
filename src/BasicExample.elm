module BasicExample exposing (..)

import Css exposing (..)
import Html exposing (Html)
import Html.Attributes as A
import Draggable
import Mouse exposing (Position)


type alias Model =
    { xy : ( Float, Float )
    , drag : Draggable.State
    }


type Msg
    = OnDragBy ( Float, Float )
    | DragMsg Draggable.Msg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { xy = ( 32, 32 ), drag = Draggable.init }
    , Cmd.none
    )


dragConfig : Draggable.Config Msg
dragConfig =
    Draggable.basicConfig OnDragBy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ xy } as model) =
    case msg of
        OnDragBy ( dx, dy ) ->
            ( { model
                | xy =
                    xy
                        |> Tuple.mapFirst ((+) dx)
                        |> Tuple.mapSecond ((+) dy)
              }
            , Cmd.none
            )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


styles =
    Css.asPairs >> A.style


view : Model -> Html Msg
view { xy } =
    let
        css =
            [ transform <| translate2 (px <| Tuple.first xy) (px <| Tuple.second xy)
            , padding (px 16)
            , backgroundColor (hex "#ccc")
            , width (px 64)
            , cursor move
            ]
    in
        Html.div
            [ styles css
            , Draggable.mouseTrigger "" DragMsg
            ]
            [ Html.text "Drag me" ]


(=>) =
    (,)
