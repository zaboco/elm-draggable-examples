module SelectList exposing (main)

import Draggable
import Draggable.Events exposing (onDragBy, onDragEnd, onDragStart, onMouseDown)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onMouseUp)


type Status
    = Fixed
    | Selecting Float


moveSelecting : Float -> Status -> Status
moveSelecting extraDelta status =
    case status of
        Fixed ->
            Fixed

        Selecting delta ->
            Selecting (delta + extraDelta)


type alias Model =
    { current : String
    , before : List String
    , after : List String
    , status : Status
    , drag : Draggable.State
    }


init : ( Model, Cmd msg )
init =
    ( model "first" [ "second", "third" ], Cmd.none )


model : String -> List String -> Model
model first rest =
    { current = first
    , before = []
    , after = rest
    , status = Fixed
    , drag = Draggable.init
    }


itemHeight =
    26


move : Float -> Model -> Model
move extraDelta model =
    { model | status = moveSelecting extraDelta model.status }
        |> normalize


normalize : Model -> Model
normalize ({ current, before, after, status } as model) =
    let
        normalizeSelecting delta =
            let
                deltaAsInt =
                    round delta

                passedItemsCount =
                    deltaAsInt // itemHeight

                deltaRemainder =
                    rem deltaAsInt itemHeight
            in
                if passedItemsCount == 0 then
                    model
                else if passedItemsCount <= -1 then
                    case List.head after of
                        Nothing ->
                            model

                        Just firstAfter ->
                            { model
                                | current = firstAfter
                                , before = current :: before
                                , after = List.drop 1 after
                                , status = Selecting <| toFloat deltaRemainder
                            }
                else
                    case List.head before of
                        Nothing ->
                            model

                        Just firstBefore ->
                            { model
                                | current = firstBefore
                                , after = current :: after
                                , before = List.drop 1 before
                                , status = Selecting <| toFloat deltaRemainder
                            }
    in
        case status of
            Fixed ->
                model

            Selecting delta ->
                normalizeSelecting delta


type Msg
    = StartSelect
    | DragList Float
    | EndSelect
    | DragMsg Draggable.Msg


dragConfig : Draggable.Config Msg
dragConfig =
    Draggable.customConfig
        [ onDragStart <| \_ -> StartSelect
        , onDragBy <| Tuple.second >> DragList
        , onDragEnd EndSelect
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartSelect ->
            { model | status = Selecting 0 } ! []

        DragList extraDelta ->
            move extraDelta model ! []

        EndSelect ->
            { model | status = Fixed } ! []

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag


fullList : Model -> List String
fullList { current, before, after } =
    (List.reverse before) ++ (current :: after)


view : Model -> Html Msg
view model =
    let
        select =
            case model.status of
                Fixed ->
                    currentItem model.current

                Selecting delta ->
                    selectList model delta
    in
        Html.div
            [ class "container" ]
            [ Html.label [ class "select-label" ] [ Html.text "Value:" ]
            , Html.div [ class "select" ] [ select ]
            , Html.span [] [ Html.text "after" ]
            , Html.div [] [ Html.text "Some other text" ]
            ]


currentItem : String -> Html Msg
currentItem value =
    Html.div
        [ class "select-current-item"
        , Draggable.mouseTrigger "" DragMsg
        ]
        [ Html.text value ]


selectList : Model -> Float -> Html msg
selectList { current, before, after } delta =
    let
        beforeDelta =
            toFloat <| itemHeight * (List.length before)

        totalDelta =
            delta - beforeDelta

        itemViews =
            List.concat
                [ List.map (selectItem False) (List.reverse before)
                , [ selectItem True current ]
                , List.map (selectItem False) after
                ]
    in
        Html.div
            [ style [ ( "transform", "translate(0," ++ toString totalDelta ++ "px)" ) ]
            ]
            [ selectItem True current
            , Html.div [ class "select-list" ] itemViews
            ]


selectItem : Bool -> String -> Html msg
selectItem selected value =
    Html.div
        [ class "select-item"
        , classList [ ( "selected", selected ) ]
        ]
        [ Html.text value ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
