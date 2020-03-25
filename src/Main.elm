module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Canvas exposing (Point, Shape, rect, shapes)
import Canvas.Settings exposing (Setting, fill, stroke)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Dict exposing (Dict)
import Html exposing (Html, button, div, p)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subs
        , view = view
        }


subs _ =
    Sub.batch
        [ onKeyDown decodeKeyPress
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]


type Arrow
    = Up
    | Down
    | Left
    | Right
    | Other


decodeKeyPress : Decode.Decoder Msg
decodeKeyPress =
    Decode.map
        (\s ->
            KeyPress
                (case s of
                    "ArrowLeft" ->
                        Left

                    "ArrowRight" ->
                        Right

                    "ArrowUp" ->
                        Up

                    "ArrowDown" ->
                        Down

                    _ ->
                        Other
                )
        )
        (Decode.field "key" Decode.string)


h : number
h =
    500


w : number
w =
    500


type alias Rect =
    { first : Point
    , second : Point
    }


type alias Shift =
    { x : Float
    , y : Float
    }


type alias Snapshot =
    { rects : Dict Int Rect
    , zoom : Float
    , shift : Shift
    }


type SnapshotId
    = SnapshotId Int


type RectId
    = RectId Int


nextRectId : RectId -> ( Int, RectId )
nextRectId (RectId n) =
    ( n, RectId (n + 1) )


nextSnapshotId : SnapshotId -> ( Int, SnapshotId )
nextSnapshotId (SnapshotId n) =
    ( n, SnapshotId (n + 1) )


type alias Model =
    { currentRect : Maybe Rect
    , currentSnapshot : SnapshotId
    , nextRectId : RectId
    , nextSnapshotId : SnapshotId
    , snapshots : Dict Int Snapshot
    , animating : Maybe Int
    }


type Msg
    = StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )
    | IncreaseZoom
    | DecreaseZoom
    | KeyPress Arrow
    | MakeNewSnapshot
    | SelectSnapshot Int
    | RunAnimation
    | AnimationFrame


init : () -> ( Model, Cmd Msg )
init _ =
    ( { nextRectId = RectId 1
      , nextSnapshotId = SnapshotId 2
      , currentSnapshot = SnapshotId 1
      , currentRect = Nothing
      , snapshots = Dict.singleton 1 { rects = Dict.empty, zoom = 1.0, shift = { x = 0, y = 0 } }
      , animating = Nothing
      }
    , Cmd.none
    )


canvasStyle : Float -> Shift -> List Setting
canvasStyle zoom shift =
    [ fill <| Color.rgba 0.0 0.0 0.0 0.0
    , stroke Color.purple
    , transform
        [ translate (w / 2) (h / 2)
        , scale zoom zoom
        , translate shift.x shift.y
        ]
    ]


globalToScreen : Float -> Shift -> Point -> Point
globalToScreen zoom shift ( x, y ) =
    ( (x + shift.x) * zoom + w / 2, (y + shift.y) * zoom + h / 2 )


screenToGlobal : Float -> Shift -> Point -> Point
screenToGlobal zoom shift ( x, y ) =
    ( (x - w / 2) / zoom - shift.x, (y - h / 2) / zoom - shift.y )


updateCurrentSnapshot : Model -> (Snapshot -> Snapshot) -> Model
updateCurrentSnapshot model f =
    let
        (SnapshotId currentSnapshotId) =
            model.currentSnapshot
    in
    { model | snapshots = Dict.update currentSnapshotId (Maybe.map f) model.snapshots }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ currentRect } as model) =
    let
        (SnapshotId currentSnapshotId) =
            model.currentSnapshot

        maybeCurrentSnapshot =
            Dict.get currentSnapshotId model.snapshots
    in
    case maybeCurrentSnapshot of
        Nothing ->
            ( model, Cmd.none )

        Just currentSnapshot ->
            ( case msg of
                StartAt screenPoint ->
                    let
                        point =
                            screenToGlobal currentSnapshot.zoom currentSnapshot.shift screenPoint
                    in
                    { model
                        | currentRect =
                            Just { first = point, second = point }
                    }

                MoveAt screenPoint ->
                    case currentRect of
                        Just { first } ->
                            { model
                                | currentRect =
                                    let
                                        point =
                                            screenToGlobal currentSnapshot.zoom currentSnapshot.shift screenPoint
                                    in
                                    Just { first = first, second = point }
                            }

                        Nothing ->
                            model

                EndAt screenPoint ->
                    case currentRect of
                        Just { first } ->
                            let
                                point =
                                    screenToGlobal currentSnapshot.zoom currentSnapshot.shift screenPoint

                                newRect =
                                    { first = first, second = point }

                                ( rectIdToInsert, nextRectIdInModel ) =
                                    nextRectId model.nextRectId

                                modelWithNewRect =
                                    updateCurrentSnapshot model (\snap -> { snap | rects = Dict.insert rectIdToInsert newRect snap.rects })
                            in
                            { modelWithNewRect
                                | nextRectId = nextRectIdInModel
                                , currentRect = Nothing
                            }

                        Nothing ->
                            model

                IncreaseZoom ->
                    updateCurrentSnapshot model (\snap -> { snap | zoom = snap.zoom * 1.1 })

                DecreaseZoom ->
                    updateCurrentSnapshot model (\snap -> { snap | zoom = snap.zoom / 1.1 })

                KeyPress arrow ->
                    let
                        ( dx, dy ) =
                            case arrow of
                                Up ->
                                    ( 0, 5 )

                                Down ->
                                    ( 0, -5 )

                                Left ->
                                    ( 5, 0 )

                                Right ->
                                    ( -5, 0 )

                                Other ->
                                    ( 0, 0 )
                    in
                    updateCurrentSnapshot model (\snap -> { snap | shift = { x = snap.shift.x + dx, y = snap.shift.y + dy } })

                MakeNewSnapshot ->
                    let
                        ( idForNewSnapshot, nextSnapshotIdInModel ) =
                            nextSnapshotId model.nextSnapshotId
                    in
                    { model
                        | snapshots = Dict.insert idForNewSnapshot currentSnapshot model.snapshots
                        , currentSnapshot = SnapshotId idForNewSnapshot
                        , nextSnapshotId = nextSnapshotIdInModel
                        , currentRect = Nothing
                    }

                SelectSnapshot id ->
                    { model | currentSnapshot = SnapshotId id, currentRect = Nothing }

                RunAnimation ->
                    { model
                        | animating = Just -60
                    }

                AnimationFrame ->
                    case model.animating of
                        Just f ->
                            if f == Dict.size model.snapshots * 60 then
                                { model | animating = Nothing }

                            else
                                { model | animating = Just (f + 1) }

                        Nothing ->
                            model
            , Cmd.none
            )


mouseDown : Decode.Decoder Msg
mouseDown =
    Decode.map2 (\x y -> StartAt ( toFloat x, toFloat y ))
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


mouseUp : Decode.Decoder Msg
mouseUp =
    Decode.map2 (\x y -> EndAt ( toFloat x, toFloat y ))
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


mouseMove : Decode.Decoder Msg
mouseMove =
    Decode.map2 (\x y -> MoveAt ( toFloat x, toFloat y ))
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


popIn : Rect -> Float -> Rect
popIn { first, second } t =
    let
        -- todo: add easing
        s =
            t * t * t

        ( ( x0, y0 ), ( x1, y1 ) ) =
            ( first, second )

        centerX =
            (x0 + x1) / 2

        centerY =
            (y0 + y1) / 2

        tfirst =
            ( x0 * s + centerX * (1 - s)
            , y0 * s + centerY * (1 - s)
            )

        tsecond =
            ( x1 * s + centerX * (1 - s)
            , y1 * s + centerY * (1 - s)
            )
    in
    { first = tfirst, second = tsecond }


popOut : Rect -> Float -> Rect
popOut r t =
    popIn r (1 - t)


rectToCanvas : Rect -> Shape
rectToCanvas { first, second } =
    let
        ( x1, y1 ) =
            first

        ( x2, y2 ) =
            second
    in
    rect first (x2 - x1) (y2 - y1)


view : Model -> Html Msg
view { currentRect, snapshots, animating, currentSnapshot } =
    let
        (SnapshotId currentSnapshotId) =
            currentSnapshot

        { rects, shift, zoom } =
            case animating of
                Just frame ->
                    let
                        f60 =
                            toFloat frame / 60

                        i =
                            floor <| f60

                        frac =
                            toFloat (modBy 60 frame) / 60

                        fraci =
                            1 - frac

                        from =
                            List.head (List.drop i (Dict.values snapshots))

                        to =
                            List.head (List.drop (i + 1) (Dict.values snapshots))
                    in
                    case ( from, to ) of
                        ( Just f, Just t ) ->
                            let
                                sft =
                                    { x = f.shift.x * fraci + t.shift.x * frac
                                    , y = f.shift.y * fraci + t.shift.y * frac
                                    }

                                zft =
                                    f.zoom * fraci + t.zoom * frac

                                rft =
                                    Dict.merge
                                        -- out
                                        (\k fr m -> Dict.insert k (popOut fr frac) m)
                                        -- transform
                                        (\k fr tr m -> Dict.insert k fr m)
                                        -- in
                                        (\k fr m -> Dict.insert k (popIn fr frac) m)
                                        f.rects
                                        t.rects
                                        Dict.empty
                            in
                            { rects = rft, shift = sft, zoom = zft }

                        ( Just f, Nothing ) ->
                            f

                        _ ->
                            { rects = Dict.empty, shift = { x = 0, y = 0 }, zoom = 1 }

                Nothing ->
                    case Dict.get currentSnapshotId snapshots of
                        Just currentSnap ->
                            case currentRect of
                                Just curRect ->
                                    { currentSnap | rects = Dict.insert -1 curRect currentSnap.rects }

                                Nothing ->
                                    currentSnap

                        Nothing ->
                            { rects = Dict.empty, shift = { x = 0, y = 0 }, zoom = 1 }
    in
    div []
        [ p [ style "text-align" "center", style "font-size" "80%" ]
            [ Html.text "Draw something! (mouse)"
            ]
        , div [ style "border" "solid 1px black", style "width" "500px", style "height" "500px" ]
            [ Canvas.toHtml ( w, h )
                [ on "mousedown" mouseDown
                , on "mousemove" mouseMove
                , on "mouseup" mouseUp
                ]
                [ shapes [ fill Color.white, lineWidth zoom ] [ rect ( 0, 0 ) w h ], shapes (canvasStyle zoom shift) (List.map rectToCanvas <| Dict.values rects) ]
            ]
        , div []
            [ button [ onClick IncreaseZoom ] [ Html.text "Zoom in" ]
            , button [ onClick DecreaseZoom ] [ Html.text "Zoom out" ]
            , Html.text ("Zoom is " ++ String.fromFloat zoom)
            ]
        , div []
            [ button [ onClick MakeNewSnapshot ] [ Html.text "Make new slide" ]
            , Html.text (String.fromInt (Dict.size snapshots) ++ " slides so far.")
            ]
        , div [] <|
            List.map
                (\id ->
                    button
                        [ onClick (SelectSnapshot id) ]
                        [ Html.span
                            [ style "color"
                                (if id == currentSnapshotId then
                                    "blue"

                                 else
                                    "black"
                                )
                            ]
                            [ Html.text ("Snapshot " ++ String.fromInt id) ]
                        ]
                )
                (Dict.keys snapshots)
        , div []
            [ button [ onClick RunAnimation ] [ Html.text "Animate" ]
            ]
        ]
