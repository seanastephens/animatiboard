module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Canvas exposing (Point, Shape, rect, shapes)
import Canvas.Settings exposing (Setting, fill, stroke)
import Canvas.Settings.Advanced exposing (scale, transform, translate)
import Color
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
    { rects : List Shape
    , zoom : Float
    , shift : Shift
    }


type alias Model =
    { rects : List Shape
    , currentRect : Maybe ( Rect, Shape )
    , zoom : Float
    , shift : Shift
    , snapshots : List Snapshot
    , animating : Maybe Int
    }


type Msg
    = StartAt ( Float, Float )
    | MoveAt ( Float, Float )
    | EndAt ( Float, Float )
    | IncreaseZoom
    | DecreaseZoom
    | KeyPress Arrow
    | TakeSnapshot
    | RunAnimation
    | AnimationFrame


init : () -> ( Model, Cmd Msg )
init _ =
    ( { rects = []
      , currentRect = Nothing
      , zoom = 1.0
      , shift = { x = 0, y = 0 }
      , snapshots = []
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ currentRect } as model) =
    ( case msg of
        StartAt screenPoint ->
            let
                point =
                    screenToGlobal model.zoom model.shift screenPoint
            in
            { model
                | currentRect =
                    Just
                        ( { first = point, second = point }
                        , rect point 0 0
                        )
            }

        MoveAt screenPoint ->
            case currentRect of
                Just ( { first }, _ ) ->
                    { model
                        | currentRect =
                            let
                                point =
                                    screenToGlobal model.zoom model.shift screenPoint

                                ( x1, y1 ) =
                                    first

                                ( x2, y2 ) =
                                    point
                            in
                            Just
                                ( { first = first, second = point }
                                , rect first (x2 - x1) (y2 - y1)
                                )
                    }

                Nothing ->
                    model

        EndAt screenPoint ->
            case currentRect of
                Just ( { first }, _ ) ->
                    let
                        point =
                            screenToGlobal model.zoom model.shift screenPoint

                        ( x1, y1 ) =
                            first

                        ( x2, y2 ) =
                            point
                    in
                    { model | rects = rect first (x2 - x1) (y2 - y1) :: model.rects, currentRect = Nothing }

                Nothing ->
                    model

        IncreaseZoom ->
            { model | zoom = model.zoom * 1.1 }

        DecreaseZoom ->
            { model | zoom = model.zoom / 1.1 }

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
            { model | shift = { x = model.shift.x + dx, y = model.shift.y + dy } }

        TakeSnapshot ->
            { model
                | snapshots =
                    model.snapshots
                        ++ [ { rects = model.rects
                             , zoom = model.zoom
                             , shift = model.shift
                             }
                           ]
            }

        RunAnimation ->
            { model
                | animating = Just -60
            }

        AnimationFrame ->
            case model.animating of
                Just f ->
                    if f == List.length model.snapshots * 60 then
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


view : Model -> Html Msg
view { rects, currentRect, zoom, shift, snapshots, animating } =
    let
        ( r, s, z ) =
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
                            List.head (List.drop i snapshots)

                        to =
                            List.head (List.drop (i + 1) snapshots)
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
                            in
                            ( f.rects, sft, zft )

                        ( Just f, Nothing ) ->
                            ( f.rects, f.shift, f.zoom  )

                        _ ->
                            ( [], { x = 0, y = 0 }, 1 )

                Nothing ->
                    case currentRect of
                        Just ( _, cur ) ->
                            ( cur :: rects, shift, zoom )

                        Nothing ->
                            ( rects, shift, zoom )
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
                [ shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ], shapes (canvasStyle z s) r ]
            ]
        , div []
            [ button [ onClick IncreaseZoom ] [ Html.text "Zoom in" ]
            , button [ onClick DecreaseZoom ] [ Html.text "Zoom out" ]
            , Html.text ("Zoom is " ++ String.fromFloat zoom)
            ]
        , div []
            [ button [ onClick TakeSnapshot ] [ Html.text "Save" ]
            , Html.text (String.fromInt (List.length snapshots) ++ " snapshots")
            ]
        , div []
            [ button [ onClick RunAnimation ] [ Html.text "Animate" ]
            ]
        ]
