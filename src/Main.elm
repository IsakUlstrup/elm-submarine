module Main exposing (Model, Msg, Submarine, main)

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
            submarine.throttle + (diff * dt * 0.003) |> clamp -1 1
        , rudder =
            let
                diff : Float
                diff =
                    submarine.rudderInput - submarine.rudder
            in
            submarine.rudder + (diff * dt * 0.003) |> clamp -1 1
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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Particle.new Vector2.zero 100) (Submarine 0 0 0 0), Cmd.none )



-- UPDATE


type Msg
    = Tick Float
    | RudderInput Float
    | ThrottleInput Float
    | KeyDown String
    | KeyUp String


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
                "ArrowUp" ->
                    { model | submarineState = setThrottleInput 1 model.submarineState }

                "ArrowLeft" ->
                    { model | submarineState = setRudderInput -1 model.submarineState }

                "ArrowRight" ->
                    { model | submarineState = setRudderInput 1 model.submarineState }

                _ ->
                    model
            , Cmd.none
            )

        KeyUp key ->
            ( case key of
                "ArrowUp" ->
                    { model | submarineState = setThrottleInput 0 model.submarineState }

                "ArrowLeft" ->
                    { model | submarineState = setRudderInput 0 model.submarineState }

                "ArrowRight" ->
                    { model | submarineState = setRudderInput 0 model.submarineState }

                _ ->
                    model
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


particleTransform : Particle -> Svg.Attribute msg
particleTransform particle =
    Svg.Attributes.transform
        ("translate("
            ++ String.fromInt (round particle.position.x)
            ++ ", "
            -- convert from cartesian coordinates to svg coordinates
            ++ String.fromInt (round particle.position.y)
            ++ ") rotate("
            -- ++ String.fromFloat (Vector2.angleDegrees particle.orientation)
            ++ "0"
            ++ ")"
        )


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
        [ particleTransform particle
        ]
        [ Svg.circle
            [ Svg.Attributes.r "20"
            , Svg.Attributes.fill "white"
            ]
            []
        , Svg.g
            [ Svg.Attributes.transform "translate(0, -50)"
            , Svg.Attributes.fontSize "0.8rem"
            , Svg.Attributes.stroke "white"
            , Svg.Attributes.strokeWidth "0.3"
            ]
            [ Svg.text_
                [ Svg.Attributes.fill "red"
                , Svg.Attributes.transform "translate(0, -30)"
                ]
                [ Svg.text "orientation" ]
            , Svg.text_
                [ Svg.Attributes.fill "green"
                , Svg.Attributes.transform "translate(0, -20)"
                ]
                [ Svg.text "orthogonal orientation" ]
            , Svg.text_
                [ Svg.Attributes.fill "orange"
                , Svg.Attributes.transform "translate(0, -10)"
                ]
                [ Svg.text "velocity" ]
            ]
        , Svg.g
            [ Svg.Attributes.strokeWidth "3"
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
            ]
        ]


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


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        [ Html.section [ Html.Attributes.class "sidebar" ]
            [ Html.label [ Html.Attributes.for "rudder-input" ] [ Html.text ("Rudder input: " ++ prettyFloat model.submarineState.rudderInput) ]
            , Html.input
                [ Html.Attributes.id "rudder-input"
                , Html.Attributes.value (String.fromFloat model.submarineState.rudderInput)
                , Html.Attributes.type_ "range"
                , Html.Attributes.min "-1"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.1"
                , Html.Events.onInput (String.toFloat >> Maybe.withDefault model.submarineState.rudderInput >> RudderInput)
                ]
                []
            , Html.label [ Html.Attributes.for "throttle-input" ] [ Html.text ("Throttle input: " ++ String.fromFloat model.submarineState.throttleInput) ]
            , Html.input
                [ Html.Attributes.id "throttle-input"
                , Html.Attributes.value (String.fromFloat model.submarineState.throttleInput)
                , Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.1"
                , Html.Events.onInput (String.toFloat >> Maybe.withDefault model.submarineState.throttleInput >> ThrottleInput)
                ]
                []
            , Html.p []
                [ Html.text "Position: "
                , Html.text (prettyFloat model.submarineParticle.position.x)
                , Html.text ", "
                , Html.text (prettyFloat model.submarineParticle.position.y)
                ]
            , Html.p []
                [ Html.text "Rotation (deg): "
                , Html.text (prettyFloat (Vector2.angleDegrees model.submarineParticle.orientation))
                ]
            , Html.p []
                [ Html.text "Rotation (rad): "
                , Html.text (prettyFloat (Vector2.angleRadian model.submarineParticle.orientation))
                ]
            , Html.p []
                [ Html.text "Orientation vector: "
                , Html.text (prettyFloat model.submarineParticle.orientation.x)
                , Html.text ", "
                , Html.text (prettyFloat model.submarineParticle.orientation.y)
                ]
            , Html.p []
                [ Html.text "Velocity: "
                , Html.text (model.submarineParticle |> Particle.velocity |> Vector2.magnitude |> prettyFloat)
                ]
            , Html.p []
                [ Html.text "Throttle: "
                , Html.text (model.submarineState.throttle |> prettyFloat)
                ]
            , Html.p []
                [ Html.text "Rudder: "
                , Html.text (model.submarineState.rudder |> prettyFloat)
                ]
            , Svg.svg
                [ Svg.Attributes.viewBox "-50 -50 100 100"
                ]
                [ viewCompass model.submarineParticle.orientation ]
            ]
        , Html.section [ Html.Attributes.class "game-view" ]
            [ Svg.svg
                [ Svg.Attributes.viewBox "-500 -500 1000 1000"
                , Svg.Attributes.id "game-view"
                ]
                [ viewSubmarine ( model.submarineParticle, model.submarineState ) ]
            ]
        ]



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
