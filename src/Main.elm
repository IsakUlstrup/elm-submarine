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



-- SUBMARINE


type alias Submarine =
    { throttle : Float
    , rudder : Float
    , rudderInput : Float
    , throttleInput : Float
    }


tickControls : Float -> Submarine -> Submarine
tickControls dt submarine =
    { submarine
        | throttle =
            let
                diff : Float
                diff =
                    submarine.throttleInput - submarine.throttle
            in
            submarine.throttle + (diff * dt * 0.002) |> clamp -1 1
        , rudder =
            let
                diff : Float
                diff =
                    submarine.rudderInput - submarine.rudder
            in
            submarine.rudder + (diff * dt * 0.0015) |> clamp -1 1
    }


setThrottleInput : Float -> Submarine -> Submarine
setThrottleInput throttle submarine =
    { submarine | throttleInput = throttle |> clamp -1 1 }


setRudderInput : Float -> Submarine -> Submarine
setRudderInput r submarine =
    { submarine | rudderInput = r |> clamp -1 1 }



-- MODEL UPDATE HELPERS


applyThrust : Model -> Model
applyThrust model =
    let
        enginePower : Float
        enginePower =
            0.01

        force : Vector2
        force =
            model.submarineParticle.orientation
                |> Vector2.scale (model.submarineState.throttle * enginePower)
    in
    { model
        | submarineParticle = Particle.applyForce force model.submarineParticle
    }


rudderForce : Model -> Model
rudderForce model =
    let
        force =
            Vector2.orthogonal model.submarineParticle.orientation
                |> Vector2.scale model.submarineState.rudder
                |> Vector2.scale (Particle.velocity model.submarineParticle |> Vector2.magnitude |> min 1)
                |> Vector2.scale 0.01

        frictionForce =
            force
                |> Vector2.scale
                    ((Particle.velocity model.submarineParticle |> Vector2.magnitude) - Vector2.magnitude force)
                |> Vector2.scale -1
    in
    { model
        | submarineParticle =
            model.submarineParticle
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
        | submarineParticle =
            Particle.applyRotationalForce
                ((model.submarineParticle
                    |> Particle.velocity
                    |> Vector2.magnitude
                 )
                    * model.submarineState.rudder
                    * steeringAuthority
                )
                model.submarineParticle
    }


friction : Model -> Model
friction model =
    { model
        | submarineParticle =
            model.submarineParticle
                |> Particle.applyRotationalForce -(model.submarineParticle.rotationVelocity * 0.1)
                |> Particle.applyForce (Particle.velocity model.submarineParticle |> Vector2.scale -0.002)
    }


controlsUpdate : Float -> Model -> Model
controlsUpdate dt model =
    { model
        | submarineState = tickControls dt model.submarineState
    }


stepParticle : Float -> Model -> Model
stepParticle dt model =
    { model | submarineParticle = Particle.step dt model.submarineParticle }



-- MODEL


type alias Model =
    { submarineParticle : Particle
    , submarineState : Submarine
    , slots : Array (Maybe Module)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Particle.new Vector2.zero 100)
        (Submarine 0 0 0 0)
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
            ( { model | submarineState = setRudderInput r model.submarineState }
            , Cmd.none
            )

        ThrottleInput throttle ->
            ( { model | submarineState = setThrottleInput throttle model.submarineState }
            , Cmd.none
            )

        KeyDown key ->
            ( case key of
                "w" ->
                    { model | submarineState = setThrottleInput 1 model.submarineState }

                "a" ->
                    { model | submarineState = setRudderInput -1 model.submarineState }

                "d" ->
                    { model | submarineState = setRudderInput 1 model.submarineState }

                _ ->
                    model
            , Cmd.none
            )

        KeyUp key ->
            ( case key of
                "w" ->
                    { model | submarineState = setThrottleInput 0 model.submarineState }

                "a" ->
                    { model | submarineState = setRudderInput 0 model.submarineState }

                "d" ->
                    { model | submarineState = setRudderInput 0 model.submarineState }

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


viewSubmarine : ( Particle, Submarine ) -> Svg msg
viewSubmarine ( particle, submarine ) =
    Svg.g
        []
        [ Svg.circle
            [ Svg.Attributes.r (String.fromFloat particle.radius)
            , Svg.Attributes.fill "white"
            ]
            []
        , Svg.g
            [ Svg.Attributes.strokeWidth "7"
            , Svg.Attributes.strokeLinecap "round"
            ]
            [ viewVector
                [ Svg.Attributes.stroke "red" ]
                particle.orientation
            , viewVector
                [ Svg.Attributes.stroke "green" ]
                (Vector2.orthogonal particle.orientation |> Vector2.scale -submarine.rudder)
            , viewVector
                [ Svg.Attributes.stroke "orange" ]
                (particle |> Particle.velocity)
            , viewVector
                [ Svg.Attributes.stroke "cyan" ]
                (particle.orientation |> Vector2.scale -1 |> Vector2.rotate -submarine.rudder)
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


viewMovementDebug : Submarine -> Particle -> Html msg
viewMovementDebug submarine particle =
    Svg.svg
        [ Svg.Attributes.viewBox "-250 -250 500 500"
        ]
        [ viewGrid particle.position
        , viewSubmarine ( particle, submarine )
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
                [ Html.Attributes.value (String.fromFloat (submarine.rudder |> min 0 |> abs))
                , Html.Attributes.style "transform" "rotate(180deg)"
                ]
                []
            , Html.meter
                [ Html.Attributes.value (String.fromFloat submarine.rudder)
                ]
                []
            ]
        , Html.h1 [] [ Html.text "Throttle" ]
        , Html.meter
            [ Html.Attributes.value (String.fromFloat submarine.throttle)
            ]
            []
        ]


viewCompassModule : Particle -> Html Msg
viewCompassModule particle =
    Html.div [ Svg.Attributes.class "module" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "-50 -50 100 100"
            ]
            [ Svg.Lazy.lazy viewCompass particle.orientation ]
        ]


viewStateModule : Particle -> Submarine -> Html msg
viewStateModule particle submarine =
    Html.div [ Html.Attributes.class "module" ]
        [ Html.p []
            [ Html.text "Position: "
            , Html.text (prettyFloat particle.position.x)
            , Html.text ", "
            , Html.text (prettyFloat particle.position.y)
            ]
        , Html.p []
            [ Html.text "Rotation (deg): "
            , Html.text (prettyFloat (Vector2.angleDegrees particle.orientation))
            ]
        , Html.p []
            [ Html.text "Rotation (rad): "
            , Html.text (prettyFloat (Vector2.angleRadian particle.orientation))
            ]
        , Html.p []
            [ Html.text "Velocity: "
            , Html.text (particle |> Particle.velocity |> Vector2.magnitude |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Throttle: "
            , Html.text (submarine.throttle |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Rudder: "
            , Html.text (submarine.rudder |> prettyFloat)
            ]
        ]


viewMovementModule : Particle -> Submarine -> Html msg
viewMovementModule particle submarine =
    Html.div [ Svg.Attributes.class "module", Svg.Attributes.class "fill" ]
        [ Svg.Lazy.lazy2 viewMovementDebug submarine particle
        ]


viewModule : Particle -> Submarine -> Module -> Html Msg
viewModule particle submarine m =
    case m of
        InputButtons ->
            viewInputButtons

        ControlsView ->
            viewControls submarine

        StateView ->
            viewStateModule particle submarine

        Compass ->
            viewCompassModule particle

        MovementDebug ->
            viewMovementModule particle submarine


viewSlot : Particle -> Submarine -> ( Int, Maybe Module ) -> Html Msg
viewSlot particle submarine ( index, slot ) =
    Html.div [ Html.Attributes.class "slot" ]
        (case slot of
            Just m ->
                [ viewModule particle submarine m ]

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
            |> List.map (viewSlot model.submarineParticle model.submarineState)
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
