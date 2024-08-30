module Main exposing (Model, Msg, Submarine, main)

import Array exposing (Array)
import Browser
import Browser.Events
import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import SubmarineState exposing (SubmarineState)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Lazy



-- MODULE


type Module
    = InputButtons
    | ControlsView
    | StateView
    | Compass
    | MovementDebug


type alias Submarine =
    Particle SubmarineState



-- MODEL UPDATE HELPERS


applyThrust : Model -> Model
applyThrust model =
    let
        enginePower : Float
        enginePower =
            0.01

        force : Vector2
        force =
            model.submarine.orientation
                |> Vector2.scale (model.submarine.state.throttle * enginePower)
    in
    { model
        | submarine = Particle.applyForce force model.submarine
    }


rudderForce : Model -> Model
rudderForce model =
    let
        force =
            Vector2.orthogonal model.submarine.orientation
                |> Vector2.scale model.submarine.state.rudder
                |> Vector2.scale (Particle.velocity model.submarine |> Vector2.magnitude |> min 1)
                |> Vector2.scale 0.01

        frictionForce =
            force
                |> Vector2.scale
                    ((Particle.velocity model.submarine |> Vector2.magnitude) - Vector2.magnitude force)
                |> Vector2.scale -1
    in
    { model
        | submarine =
            model.submarine
                |> Particle.applyForce force
                |> Particle.applyForce frictionForce
    }


applyRotation : Model -> Model
applyRotation model =
    let
        steeringAuthority : Float
        steeringAuthority =
            0.001
    in
    { model
        | submarine =
            Particle.applyRotationalForce
                ((model.submarine
                    |> Particle.velocity
                    |> Vector2.magnitude
                 )
                    * model.submarine.state.rudder
                    * steeringAuthority
                )
                model.submarine
    }


friction : Model -> Model
friction model =
    { model
        | submarine =
            model.submarine
                |> Particle.applyRotationalForce -(model.submarine.rotationVelocity * 0.1)
                |> Particle.applyForce (Particle.velocity model.submarine |> Vector2.scale -0.002)
    }


controlsUpdate : Float -> Model -> Model
controlsUpdate dt model =
    { model
        | submarine = model.submarine |> Particle.updateState (SubmarineState.tickControls dt)
    }


stepParticle : Float -> Model -> Model
stepParticle dt model =
    { model | submarine = Particle.step dt model.submarine }



-- MODEL


type alias Model =
    { submarine : Submarine
    , slots : Array (Maybe Module)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Particle.new SubmarineState.new Vector2.zero 100)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> stepParticle dt
                |> controlsUpdate dt
                |> applyThrust
                |> rudderForce
                |> applyRotation
                |> friction
            , Cmd.none
            )

        RudderInput r ->
            ( { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setRudderInput r) }
            , Cmd.none
            )

        ThrottleInput throttle ->
            ( { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setThrottleInput throttle) }
            , Cmd.none
            )

        KeyDown key ->
            ( case key of
                "w" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setThrottleInput 1) }

                "a" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setRudderInput -1) }

                "d" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setRudderInput 1) }

                _ ->
                    model
            , Cmd.none
            )

        KeyUp key ->
            ( case key of
                "w" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setThrottleInput 0) }

                "a" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setRudderInput 0) }

                "d" ->
                    { model | submarine = model.submarine |> Particle.updateState (SubmarineState.setRudderInput 0) }

                _ ->
                    model
            , Cmd.none
            )

        ClickedAddModule index mod ->
            ( { model | slots = Array.set index (Just mod) model.slots }
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


viewCompass : Vector2 -> Svg msg
viewCompass bearing =
    let
        ( x2, y2 ) =
            ( bearing.x * 35, bearing.y * 35 )
    in
    Svg.g []
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


viewMovementDebug : Submarine -> Html msg
viewMovementDebug submarine =
    Svg.svg
        [ Svg.Attributes.viewBox "-250 -250 500 500"
        ]
        [ viewGrid submarine.position
        , viewSubmarine submarine
        ]


viewInputButtons : Html Msg
viewInputButtons =
    Html.div [ Svg.Attributes.class "module" ]
        [ Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (RudderInput -1))
            , Html.Events.on "pointerup" (Decode.succeed (RudderInput 0))
            , Html.Events.on "pointerleave" (Decode.succeed (RudderInput 0))
            ]
            [ Html.text "Port" ]
        , Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (ThrottleInput 1))
            , Html.Events.on "pointerup" (Decode.succeed (ThrottleInput 0))
            , Html.Events.on "pointerleave" (Decode.succeed (ThrottleInput 0))
            ]
            [ Html.text "Forwards" ]
        , Html.button
            [ Html.Events.on "pointerdown" (Decode.succeed (RudderInput 1))
            , Html.Events.on "pointerup" (Decode.succeed (RudderInput 0))
            , Html.Events.on "pointerleave" (Decode.succeed (RudderInput 0))
            ]
            [ Html.text "Starboard" ]
        ]


viewControls : Submarine -> Html Msg
viewControls submarine =
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


viewCompassModule : Submarine -> Html Msg
viewCompassModule submarine =
    Html.div [ Svg.Attributes.class "module" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-50 -50 100 100"
            ]
            [ Svg.Lazy.lazy viewCompass submarine.orientation ]
        ]


viewStateModule : Submarine -> Html msg
viewStateModule submarine =
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


viewMovementModule : Submarine -> Html msg
viewMovementModule submarine =
    Html.div [ Svg.Attributes.class "module", Svg.Attributes.class "fill" ]
        [ Svg.Lazy.lazy viewMovementDebug submarine
        ]


viewModule : Submarine -> Module -> Html Msg
viewModule submarine m =
    case m of
        InputButtons ->
            viewInputButtons

        ControlsView ->
            viewControls submarine

        StateView ->
            viewStateModule submarine

        Compass ->
            viewCompassModule submarine

        MovementDebug ->
            viewMovementModule submarine


viewSlot : Submarine -> ( Int, Maybe Module ) -> Html Msg
viewSlot submarine ( index, slot ) =
    Html.div [ Html.Attributes.class "slot" ]
        (case slot of
            Just m ->
                [ viewModule submarine m ]

            Nothing ->
                [ Html.button [ Html.Attributes.attribute "popovertarget" ("add-module-" ++ String.fromInt index) ] [ Html.text "Install module" ]
                , Html.div
                    [ Html.Attributes.attribute "popover" ""
                    , Html.Attributes.id ("add-module-" ++ String.fromInt index)
                    , Html.Attributes.class "add-module-popup"
                    ]
                    [ Html.h3 [] [ Html.text ("Slot #" ++ String.fromInt index) ]
                    , Html.ul [ Html.Attributes.class "modules" ]
                        [ Html.li [ Html.Events.onClick (ClickedAddModule index InputButtons) ] [ Html.text "Input buttons" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index ControlsView) ] [ Html.text "Controls state" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index StateView) ] [ Html.text "State view" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index Compass) ] [ Html.text "Compass" ]
                        , Html.li [ Html.Events.onClick (ClickedAddModule index MovementDebug) ] [ Html.text "Movement" ]
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
