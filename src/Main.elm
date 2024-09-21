module Main exposing (Model, Module, Msg, main)

import Browser
import Browser.Events
import Controls exposing (Controls)
import Dict exposing (Dict)
import Engine.Quaternion as Quaternion
import Engine.Rigidbody as Rigidbody exposing (Rigidbody)
import Engine.Vector as Vector exposing (Vector)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Timing exposing (Timing)



-- FORCES
-- rotationForces : Controls -> Particle -> Particle
-- rotationForces controls particle =
--     let
--         velocity =
--             Particle.velocity particle
--         rudderAngle =
--             particle.orientation - controls.rudder
--         angleDelta =
--             Vector2.angleRadian velocity - rudderAngle
--     in
--     particle
--         |> Particle.applyRotationalForce (angleDelta * Vector2.magnitude velocity * 0.05)
--         |> Particle.applyRotationalForce -(particle.rotationVelocity * 10)
--         |> Particle.applyForce (Particle.forwards particle |> Vector2.scale (Vector2.magnitude velocity * 0.1))
--         |> Particle.applyForce (velocity |> Vector2.scale -0.1)
-- thrustForce : Controls -> Particle -> Particle
-- thrustForce controls particle =
--     let
--         force =
--             particle
--                 |> Particle.forwards
--                 |> Vector2.scale (controls.enginePower * controls.throttle)
--     in
--     Particle.applyForce force particle


rotation : Float -> Controls -> Rigidbody -> Rigidbody
rotation dt controls rigidbody =
    rigidbody
        |> Rigidbody.rotate (Rigidbody.zRotation (controls.rudder * dt * 0.1 |> degrees))


movement : Float -> Controls -> Rigidbody -> Rigidbody
movement dt controls rigidbody =
    rigidbody
        |> Rigidbody.translateRelative (Vector.right |> Vector.scale (controls.throttle * controls.enginePower * dt))



-- MODULE


type Module
    = SteeringButtons
    | ThrottleButtons
    | PhysicsDebug
    | StateDump



-- MODEL


type alias Model =
    { particle : Rigidbody
    , modules : List Module
    , controls : Controls
    , timing : Timing
    , keybinds : Dict String ( Msg, Msg )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Rigidbody.new Vector.zero 2000
         -- |> Particle.setOrientation (pi / 2)
         -- |> Particle.applyForce (Vector2.new 0 30)
        )
        [ SteeringButtons
        , ThrottleButtons
        , PhysicsDebug
        , StateDump
        ]
        (Controls.new 1 0.1)
        0
        (Dict.fromList
            [ ( "w", ( ThrottleInput 1, ThrottleInput 0 ) )
            , ( "s", ( ThrottleInput -1, ThrottleInput 0 ) )
            , ( "a", ( SteeringInput -1, SteeringInput 0 ) )
            , ( "d", ( SteeringInput 1, SteeringInput 0 ) )
            ]
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | SteeringInput Float
    | ThrottleInput Float


physicsUpdate : Controls -> Float -> Rigidbody -> Rigidbody
physicsUpdate controls dt rigidbody =
    rigidbody
        |> Rigidbody.step dt
        |> rotation dt controls
        |> movement dt controls



-- |> rotationForces controls
-- |> thrustForce controls
-- |> Submarine.controlsUpdate dt
-- |> Submarine.applyThrust
-- |> Submarine.rudderForce
-- |> Submarine.applyRotation
-- |> Submarine.friction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( newTiming, newParticle ) =
                    Timing.fixedUpdate (physicsUpdate model.controls) dt ( model.timing, model.particle )
            in
            ( { model
                | particle = newParticle
                , timing = newTiming
                , controls = Controls.tick dt model.controls
              }
            , Cmd.none
            )

        SteeringInput input ->
            ( { model | controls = Controls.setRudderInput input model.controls }
            , Cmd.none
            )

        ThrottleInput input ->
            ( { model | controls = Controls.setThrottleInput input model.controls }
            , Cmd.none
            )



-- VIEW


viewVector : List (Svg.Attribute msg) -> Vector -> Svg msg
viewVector attrs vector =
    let
        to : Vector
        to =
            Vector.scale 50 vector
    in
    Svg.line
        ([ Svg.Attributes.x1 "0"
         , Svg.Attributes.y1 "0"
         , Svg.Attributes.x2 (String.fromFloat to.x)
         , Svg.Attributes.y2 (String.fromFloat to.y)
         ]
            ++ attrs
        )
        []


viewGrid : Vector -> Svg msg
viewGrid pos =
    let
        verticalLine : Int -> Svg msg
        verticalLine x =
            Svg.line
                [ Svg.Attributes.x1 (String.fromInt x)
                , Svg.Attributes.y1 "-500"
                , Svg.Attributes.x2 (String.fromInt x)
                , Svg.Attributes.y2 "500"
                ]
                []

        horizontalLine : Int -> Svg msg
        horizontalLine y =
            Svg.line
                [ Svg.Attributes.x1 "-500"
                , Svg.Attributes.y1 (String.fromInt y)
                , Svg.Attributes.x2 "500"
                , Svg.Attributes.y2 (String.fromInt y)
                ]
                []

        spacing : Int
        spacing =
            200
    in
    Svg.g
        [ Svg.Attributes.stroke "#262626"
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


prettyFloat : Float -> String
prettyFloat n =
    case n |> String.fromFloat |> String.split "." of
        [ x ] ->
            x ++ ".00"

        x :: xs ->
            x ++ "." ++ (xs |> String.concat |> String.left 2)

        [] ->
            "E: " ++ String.fromFloat n


viewPhysicsDebug : Controls -> Rigidbody -> Html msg
viewPhysicsDebug _ rigidbody =
    Html.div []
        [ Svg.svg
            [ Svg.Attributes.viewBox "-250 -250 500 500"
            , Svg.Attributes.class "movement-debug"

            -- flip svg y axis so we can use cartesian coordinates
            --, Svg.Attributes.transform "matrix(1 0 0 -1 0 0)"
            ]
            [ viewGrid rigidbody.position
            , Svg.g
                [ Svg.Attributes.strokeWidth "7"
                , Svg.Attributes.stroke "white"
                , Svg.Attributes.strokeLinecap "round"
                ]
                [ viewVector
                    [ Svg.Attributes.stroke "orange" ]
                    (Rigidbody.velocity rigidbody)
                , Svg.g [ Svg.Attributes.transform ("rotate(" ++ String.fromFloat (Quaternion.zToEuler rigidbody.orientation * 180 / pi) ++ ")") ]
                    [ Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 "100"
                        , Svg.Attributes.y2 "0"
                        , Svg.Attributes.stroke "red"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 "0"
                        , Svg.Attributes.y2 "-100"
                        , Svg.Attributes.stroke "green"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 "0"
                        , Svg.Attributes.y2 "0"
                        , Svg.Attributes.stroke "blue"
                        ]
                        []
                    ]
                ]
            ]
        ]


viewSteeringButtons : Controls -> Html Msg
viewSteeringButtons controls =
    Html.div []
        [ Html.h1 [] [ Html.text "Rudder" ]
        , Html.div [ Html.Attributes.class "button-group" ]
            [ Html.button
                [ Html.Events.onMouseDown (SteeringInput -1)
                , Html.Events.onMouseUp (SteeringInput 0)
                ]
                [ Html.text "Left" ]
            , Html.button
                [ Html.Events.onMouseDown (SteeringInput 1)
                , Html.Events.onMouseUp (SteeringInput 0)
                ]
                [ Html.text "Right" ]
            ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "-1"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.1"
            , Html.Attributes.value (String.fromFloat controls.rudderInput)
            , Html.Events.onInput (String.toFloat >> Maybe.withDefault controls.rudderInput >> SteeringInput)
            , Html.Events.onMouseUp (SteeringInput 0)
            ]
            []
        , Html.meter
            [ Html.Attributes.min "-1"
            , Html.Attributes.max "1"
            , Html.Attributes.value (String.fromFloat controls.rudder)
            ]
            []
        ]


viewThrottleButtons : Controls -> Html Msg
viewThrottleButtons controls =
    Html.div []
        [ Html.h1 [] [ Html.text "Throttle" ]
        , Html.div [ Html.Attributes.class "button-group" ]
            [ Html.button
                [ Html.Events.onMouseDown (ThrottleInput -1)
                , Html.Events.onMouseUp (ThrottleInput 0)
                ]
                [ Html.text "Backwards" ]
            , Html.button
                [ Html.Events.onMouseDown (ThrottleInput 1)
                , Html.Events.onMouseUp (ThrottleInput 0)
                ]
                [ Html.text "Forwards" ]
            ]
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.min "-1"
            , Html.Attributes.max "1"
            , Html.Attributes.step "0.1"
            , Html.Attributes.value (String.fromFloat controls.throttleInput)
            , Html.Events.onInput (String.toFloat >> Maybe.withDefault controls.throttleInput >> ThrottleInput)
            , Html.Events.onMouseUp (ThrottleInput 0)
            ]
            []
        , Html.meter
            [ Html.Attributes.min "-1"
            , Html.Attributes.max "1"
            , Html.Attributes.value (String.fromFloat controls.throttle)
            ]
            []
        ]


viewStateDump : Rigidbody -> Html msg
viewStateDump rigidbody =
    Html.div []
        [ Html.p []
            [ Html.text "X (rad): "
            , Html.text (prettyFloat (Quaternion.xToEuler rigidbody.orientation))
            ]
        , Html.p []
            [ Html.text "Y (rad): "
            , Html.text (prettyFloat (Quaternion.yToEuler rigidbody.orientation))
            ]
        , Html.p []
            [ Html.text "Z (rad): "
            , Html.text (prettyFloat (Quaternion.zToEuler rigidbody.orientation))
            ]
        , Html.p []
            [ Html.text "x: "
            , Html.text (prettyFloat rigidbody.position.x)
            ]
        , Html.p []
            [ Html.text "y: "
            , Html.text (prettyFloat rigidbody.position.y)
            ]
        , Html.p []
            [ Html.text "z: "
            , Html.text (prettyFloat rigidbody.position.z)
            ]
        , Html.p []
            [ Html.text "Velocity: "
            , Html.text (rigidbody |> Rigidbody.velocity |> Vector.magnitude |> prettyFloat)
            ]
        ]


view : Model -> Html Msg
view model =
    let
        viewModule : Module -> Html Msg
        viewModule m =
            case m of
                SteeringButtons ->
                    viewSteeringButtons model.controls

                ThrottleButtons ->
                    viewThrottleButtons model.controls

                PhysicsDebug ->
                    Html.Lazy.lazy2 viewPhysicsDebug model.controls model.particle

                StateDump ->
                    viewStateDump model.particle
    in
    main_ [ Html.Attributes.id "app" ]
        (List.map viewModule model.modules)



-- DECODERS


keyDecoder : Dict String ( Msg, Msg ) -> Bool -> Decoder Msg
keyDecoder binds pressed =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case Dict.get key binds of
                    Just ( downMsg, upMsg ) ->
                        if pressed then
                            Decode.succeed <| downMsg

                        else
                            Decode.succeed <| upMsg

                    Nothing ->
                        Decode.fail "unknown key bind"
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder model.keybinds True)
        , Browser.Events.onKeyUp (keyDecoder model.keybinds False)
        , Browser.Events.onAnimationFrameDelta Tick
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
