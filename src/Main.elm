module Main exposing (Model, Msg, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Engine.Particle as Particle
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Submarine exposing (Submarine)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy



-- MODULE


type Module
    = InputButtons ButtonState
    | ControlsState
    | StateDump
    | Compass
    | Movement


type alias ButtonState =
    { left : Bool
    , forwards : Bool
    , right : Bool
    }


type Direction
    = Left
    | Forwards
    | Right


updateButtonState : (ButtonState -> ButtonState) -> Module -> Module
updateButtonState f mod =
    case mod of
        InputButtons state ->
            InputButtons (f state)

        _ ->
            mod


setDirectionPressed : Direction -> Bool -> ButtonState -> ButtonState
setDirectionPressed direction pressed state =
    case direction of
        Left ->
            { state | left = pressed }

        Forwards ->
            { state | forwards = pressed }

        Right ->
            { state | right = pressed }



-- MODEL


type alias Model =
    { submarine : Submarine
    , slots : Array (Maybe Module)
    }


updateSlot : Int -> (Module -> Module) -> Model -> Model
updateSlot index f model =
    let
        slot =
            Array.get index model.slots
                |> Maybe.andThen
                    (Maybe.map f)
    in
    { model | slots = Array.set index slot model.slots }


applyModules : Model -> Model
applyModules model =
    let
        modules =
            model.slots |> Array.toList |> List.filterMap identity

        applyModule m s =
            case m of
                InputButtons state ->
                    if state.left then
                        s |> Particle.updateState (Submarine.setRudderInput -1)

                    else if state.right then
                        s |> Particle.updateState (Submarine.setRudderInput 1)

                    else if state.forwards then
                        s |> Particle.updateState (Submarine.setThrottleInput 1)

                    else
                        s
                            |> Particle.updateState (Submarine.setThrottleInput 0)
                            |> Particle.updateState (Submarine.setRudderInput 0)

                _ ->
                    s
    in
    { model | submarine = List.foldl applyModule model.submarine modules }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Particle.new Submarine.new Vector2.zero 100)
        (Array.repeat 8 Nothing)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | RudderInput Float
    | ThrottleInput Float
    | KeyDown String
    | KeyUp String
    | ClickedAddModule Int Module
    | ClickedInputButton Int Direction Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | submarine =
                    model.submarine
                        |> Submarine.stepParticle dt
                        |> Submarine.controlsUpdate dt
                        |> Submarine.applyThrust
                        |> Submarine.rudderForce
                        |> Submarine.applyRotation
                        |> Submarine.friction
              }
                |> applyModules
            , Cmd.none
            )

        RudderInput r ->
            ( { model | submarine = model.submarine |> Particle.updateState (Submarine.setRudderInput r) }
            , Cmd.none
            )

        ThrottleInput throttle ->
            ( { model | submarine = model.submarine |> Particle.updateState (Submarine.setThrottleInput throttle) }
            , Cmd.none
            )

        KeyDown key ->
            ( case key of
                "w" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setThrottleInput 1) }

                "a" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setRudderInput -1) }

                "d" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setRudderInput 1) }

                _ ->
                    model
            , Cmd.none
            )

        KeyUp key ->
            ( case key of
                "w" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setThrottleInput 0) }

                "a" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setRudderInput 0) }

                "d" ->
                    { model | submarine = model.submarine |> Particle.updateState (Submarine.setRudderInput 0) }

                _ ->
                    model
            , Cmd.none
            )

        ClickedAddModule index mod ->
            ( { model | slots = Array.set index (Just mod) model.slots }
            , Cmd.none
            )

        ClickedInputButton index direction pressed ->
            ( updateSlot index (updateButtonState <| setDirectionPressed direction pressed) model
            , Cmd.none
            )



-- VIEW


prettyFloat : Float -> String
prettyFloat f =
    case f |> String.fromFloat |> String.split "." of
        [ x ] ->
            x

        x :: xs ->
            x ++ "." ++ String.left 1 (String.concat xs)

        [] ->
            "Error"


viewVector : List (Svg.Attribute msg) -> Vector2 -> Svg msg
viewVector attrs vector =
    let
        ( x2, y2 ) =
            ( vector.x * 50, vector.y * 50 )
    in
    Svg.line
        ([ Svg.Attributes.x1 "0"
         , Svg.Attributes.y1 "0"
         , Svg.Attributes.x2 (String.fromFloat x2)
         , Svg.Attributes.y2 (String.fromFloat y2)
         ]
            ++ attrs
        )
        []


viewSubmarine : Submarine -> Svg msg
viewSubmarine submarine =
    Svg.g
        []
        [ Svg.circle
            [ Svg.Attributes.r (String.fromFloat submarine.radius)
            , Svg.Attributes.fill "white"
            ]
            []
        , Svg.g
            [ Svg.Attributes.strokeWidth "7"
            , Svg.Attributes.strokeLinecap "round"
            ]
            [ viewVector
                [ Svg.Attributes.stroke "red" ]
                submarine.orientation
            , viewVector
                [ Svg.Attributes.stroke "green" ]
                (Vector2.orthogonal submarine.orientation |> Vector2.scale -submarine.state.rudder)
            , viewVector
                [ Svg.Attributes.stroke "orange" ]
                (Particle.velocity submarine)
            , viewVector
                [ Svg.Attributes.stroke "cyan" ]
                (submarine.orientation |> Vector2.scale -1 |> Vector2.rotate -submarine.state.rudder)
            ]
        ]


viewGrid : Vector2 -> Svg msg
viewGrid pos =
    let
        verticalLine x =
            Svg.line
                [ Svg.Attributes.x1 (String.fromInt x)
                , Svg.Attributes.y1 "-500"
                , Svg.Attributes.x2 (String.fromInt x)
                , Svg.Attributes.y2 "500"
                ]
                []

        horizontalLine y =
            Svg.line
                [ Svg.Attributes.x1 "-500"
                , Svg.Attributes.y1 (String.fromInt y)
                , Svg.Attributes.x2 "500"
                , Svg.Attributes.y2 (String.fromInt y)
                ]
                []

        spacing =
            200
    in
    Svg.g
        [ Svg.Attributes.stroke "white"
        , Svg.Attributes.transform
            ("translate("
                ++ String.fromInt -(modBy spacing (round pos.x))
                ++ ", "
                ++ String.fromInt -(modBy spacing (round pos.y))
                ++ ")"
            )
        ]
        (List.range -2 2
            |> List.concatMap
                (\i ->
                    [ verticalLine (i * spacing)
                    , horizontalLine (i * spacing)
                    ]
                )
        )


viewMovementDebug : Submarine -> Html msg
viewMovementDebug submarine =
    Svg.svg
        [ Svg.Attributes.viewBox "-250 -250 500 500"
        ]
        [ viewGrid submarine.position
        , viewSubmarine submarine
        ]


viewInputButtons : Int -> ButtonState -> Html Msg
viewInputButtons index state =
    Html.div [ Svg.Attributes.class "module" ]
        [ Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (ClickedInputButton index Left True))
            , Html.Events.on "pointerup" (Decode.succeed (ClickedInputButton index Left False))
            , Html.Events.on "pointerleave" (Decode.succeed (ClickedInputButton index Left False))
            , Html.Attributes.classList [ ( "down", state.left ) ]
            ]
            [ Html.text "Port" ]
        , Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (ClickedInputButton index Forwards True))
            , Html.Events.on "pointerup" (Decode.succeed (ClickedInputButton index Forwards False))
            , Html.Events.on "pointerleave" (Decode.succeed (ClickedInputButton index Forwards False))
            , Html.Attributes.classList [ ( "down", state.forwards ) ]
            ]
            [ Html.text "Forwards" ]
        , Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (ClickedInputButton index Right True))
            , Html.Events.on "pointerup" (Decode.succeed (ClickedInputButton index Right False))
            , Html.Events.on "pointerleave" (Decode.succeed (ClickedInputButton index Right False))
            , Html.Attributes.classList [ ( "down", state.right ) ]
            ]
            [ Html.text "Starboard" ]
        ]


viewControlsState : Submarine -> Html Msg
viewControlsState submarine =
    Html.div [ Html.Attributes.class "module" ]
        [ Html.h1 [] [ Html.text "Rudder" ]
        , Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "gap" "0.5rem"
            ]
            [ Html.meter
                [ Html.Attributes.value (String.fromFloat (submarine.state.rudder |> min 0 |> abs))
                , Html.Attributes.style "transform" "rotate(180deg)"
                ]
                []
            , Html.meter
                [ Html.Attributes.value (String.fromFloat submarine.state.rudder)
                ]
                []
            ]
        , Html.h1 [] [ Html.text "Throttle" ]
        , Html.meter
            [ Html.Attributes.value (String.fromFloat submarine.state.throttle)
            ]
            []
        ]


viewCompass : Submarine -> Html Msg
viewCompass submarine =
    let
        ( x2, y2 ) =
            ( submarine.orientation.x * 35, submarine.orientation.y * 35 )
    in
    Html.div [ Svg.Attributes.class "module" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-50 -50 100 100"
            ]
            [ Svg.circle [ Svg.Attributes.r "50", Svg.Attributes.fill "white" ] []
            , Svg.g
                [ Svg.Attributes.dominantBaseline "central"
                , Svg.Attributes.textAnchor "middle"
                , Svg.Attributes.fontSize "0.5rem"
                ]
                [ Svg.text_
                    [ Svg.Attributes.y "-40" ]
                    [ Svg.text "N" ]
                , Svg.text_
                    [ Svg.Attributes.y "40" ]
                    [ Svg.text "S" ]
                , Svg.text_
                    [ Svg.Attributes.x "-40" ]
                    [ Svg.text "W" ]
                , Svg.text_
                    [ Svg.Attributes.x "40" ]
                    [ Svg.text "E" ]
                ]
            , Svg.line
                [ Svg.Attributes.x1 "0"
                , Svg.Attributes.y1 "0"
                , Svg.Attributes.x2 (String.fromFloat x2)
                , Svg.Attributes.y2 (String.fromFloat y2)
                , Svg.Attributes.stroke "red"
                , Svg.Attributes.strokeWidth "2"
                , Svg.Attributes.strokeLinecap "round"
                ]
                []
            ]
        ]


viewStateDump : Submarine -> Html msg
viewStateDump submarine =
    Html.div [ Html.Attributes.class "module" ]
        [ Html.p []
            [ Html.text "Position: "
            , Html.text (prettyFloat submarine.position.x)
            , Html.text ", "
            , Html.text (prettyFloat submarine.position.y)
            ]
        , Html.p []
            [ Html.text "Rotation (deg): "
            , Html.text (prettyFloat (Vector2.angleDegrees submarine.orientation))
            ]
        , Html.p []
            [ Html.text "Rotation (rad): "
            , Html.text (prettyFloat (Vector2.angleRadian submarine.orientation))
            ]
        , Html.p []
            [ Html.text "Velocity: "
            , Html.text (submarine |> Particle.velocity |> Vector2.magnitude |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Throttle: "
            , Html.text (submarine.state.throttle |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Rudder: "
            , Html.text (submarine.state.rudder |> prettyFloat)
            ]
        ]


viewMovement : Submarine -> Html msg
viewMovement submarine =
    Html.div [ Svg.Attributes.class "module", Svg.Attributes.class "fill" ]
        [ Svg.Lazy.lazy viewMovementDebug submarine
        ]


viewModule : Int -> Submarine -> Module -> Html Msg
viewModule index submarine m =
    case m of
        InputButtons state ->
            viewInputButtons index state

        ControlsState ->
            viewControlsState submarine

        StateDump ->
            viewStateDump submarine

        Compass ->
            viewCompass submarine

        Movement ->
            viewMovement submarine


viewSlot : Submarine -> ( Int, Maybe Module ) -> Html Msg
viewSlot submarine ( index, slot ) =
    Html.div [ Html.Attributes.class "slot" ]
        (case slot of
            Just m ->
                [ viewModule index submarine m ]

            Nothing ->
                [ Html.button [ Html.Attributes.attribute "popovertarget" ("add-module-" ++ String.fromInt index) ] [ Html.text "Install module" ]
                , Html.div
                    [ Html.Attributes.attribute "popover" ""
                    , Html.Attributes.id ("add-module-" ++ String.fromInt index)
                    , Html.Attributes.class "add-module-popup"
                    ]
                    [ Html.h3 [] [ Html.text ("Slot #" ++ String.fromInt index) ]
                    , Html.ul [ Html.Attributes.class "modules" ]
                        [ Html.li [ Html.Events.onClick (ClickedAddModule index (InputButtons { left = False, forwards = False, right = False })) ] [ Html.text "Input buttons" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index ControlsState) ] [ Html.text "Controls state" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index StateDump) ] [ Html.text "State view" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index Compass) ] [ Html.text "Compass" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index Movement) ] [ Html.text "Movement" ]
                        ]
                    ]
                ]
        )


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        (model.slots
            |> Array.toIndexedList
            |> List.map (viewSlot model.submarine)
        )



-- DECODERS


keyDecoder : (String -> msg) -> Decoder msg
keyDecoder msg =
    Decode.map msg
        (Decode.field "key" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (min 50 >> Tick)
        , Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
