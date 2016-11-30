module SortableList exposing (..)

import Array exposing (Array)
import Draggable
import Draggable.Events exposing (onDragBy, onMouseDownKeyed, onMouseUp)
import Array.Extra as ArrayX
import Html exposing (Html)
import Html.Attributes
import Css exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { items : Items
    , draggingItemIndex : Maybe Index
    , draggedDistance : Distance
    , drag : Draggable.State
    }


type alias Items =
    Array Item


type alias Item =
    { text : String
    }


type Distance
    = Distance HorizontalDistance VerticalDistance


type alias Delta =
    ( HorizontalDistance, VerticalDistance )


type alias Index =
    Int


type alias VerticalDistance =
    Float


type alias HorizontalDistance =
    Float


type Msg
    = DragBy Delta
    | StartDragging String
    | StopDragging
    | DragMsg Draggable.Msg


init : ( Model, Cmd Msg )
init =
    ( { items = Array.fromList [ item "#1 first item", item "#2 second item", item "#3 third item" ]
      , draggingItemIndex = Nothing
      , draggedDistance = noDistance
      , drag = Draggable.init
      }
    , Cmd.none
    )


item : String -> Item
item =
    Item


noDistance : Distance
noDistance =
    Distance 0 0


increaseDistance : Delta -> Distance -> Distance
increaseDistance ( dx, dy ) (Distance h v) =
    Distance (h + dx) (v + dy)


vertical : Distance -> VerticalDistance
vertical (Distance _ verticalDistance) =
    verticalDistance


dragConfig : Draggable.Config Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy (Draggable.deltaToFloats >> DragBy)
        , onMouseDownKeyed StartDragging
        , onMouseUp StopDragging
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ items, draggingItemIndex, draggedDistance } as model) =
    case msg of
        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        DragBy delta ->
            ( { model | draggedDistance = increaseDistance delta draggedDistance }, Cmd.none )

        StartDragging index ->
            ( { model | draggingItemIndex = index |> String.toInt |> Result.toMaybe }, Cmd.none )

        StopDragging ->
            ( { model
                | draggingItemIndex = Nothing
                , draggedDistance =
                    noDistance
                , items =
                    case draggingItemIndex of
                        Just oldIndex ->
                            reorderItems
                                oldIndex
                                (virtualIndex (vertical draggedDistance) oldIndex)
                                items

                        Nothing ->
                            items
              }
            , Cmd.none
            )


reorderItems : Index -> Index -> Array a -> Array a
reorderItems oldIndex newIndex items =
    let
        movedItem =
            ArrayX.getUnsafe oldIndex items
    in
        items
            |> ArrayX.removeAt oldIndex
            |> insertAt newIndex movedItem


insertAt : Index -> a -> Array a -> Array a
insertAt index newItem items =
    let
        ( before, after ) =
            ArrayX.splitAt index items
    in
        before
            |> Array.push newItem
            |> flip Array.append after


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


view : Model -> Html Msg
view { items, draggingItemIndex, draggedDistance } =
    Html.ul
        [ styles
            [ width (px 200)
            , border3 (px 1) dashed (hex "#000")
            , displayFlex
            , flexDirection column
            , padding (px 0)
            , margin (px 32)
            ]
        ]
        (Array.toList <| Array.indexedMap (itemView draggingItemIndex draggedDistance) items)


itemHeight : Float
itemHeight =
    50


itemMargin : Float
itemMargin =
    5


virtualIndex : VerticalDistance -> Index -> Index
virtualIndex verticalDistance activeIndex =
    activeIndex + floor (verticalDistance / itemHeight + 0.5)


itemView : Maybe Index -> Distance -> Index -> Item -> Html Msg
itemView activeIndex (Distance h v) index { text } =
    let
        ( dx, dy ) =
            case activeIndex of
                Just justActiveIndex ->
                    if justActiveIndex == index then
                        ( h, v )
                    else if index <= (virtualIndex v justActiveIndex) && index >= justActiveIndex then
                        ( 0, -itemHeight )
                    else if index >= (virtualIndex v justActiveIndex) && index <= justActiveIndex then
                        ( 0, itemHeight )
                    else
                        ( 0, 0 )

                Nothing ->
                    ( 0, 0 )

        transition =
            if List.member activeIndex [ Just index, Nothing ] then
                ""
            else
                "transform 200ms"

        zIndex =
            if activeIndex == Just index then
                "1"
            else
                "0"

        scaleFactor =
            if activeIndex == Just index then
                1.05
            else
                1

        shadow =
            if activeIndex == Just index then
                boxShadow3 (px 3) (px 3) (px 10)
            else
                boxShadow4 (px 3) (px 3) (px 10) (px -5)
    in
        Html.li
            [ styles
                [ displayFlex
                , boxSizing borderBox
                , transforms
                    [ translate2 (px dx) (px dy)
                    , scale scaleFactor
                    ]
                , padding2 (px 0) (px 10)
                , alignItems center
                , height (px <| itemHeight - 2 * itemMargin)
                , border3 (px 1) solid (hex "#d00")
                , margin (px itemMargin)
                , cursor nsResize
                , backgroundColor (hex "#fff")
                , shadow
                , property "-webkit-user-select" "none"
                , property "transition" transition
                , property "z-index" zIndex
                ]
            , Draggable.mouseTrigger (toString index) DragMsg
            ]
            [ Html.text text ]
