module Main exposing (Model, Module, Msg, main)

import Browser
import Browser.Events
import Controls exposing (Controls)
import Dict exposing (Dict)
import Engine.Quaternion as Quaternion exposing (Quaternion)
import Engine.Rigidbody as Rigidbody exposing (Rigidbody)
import Engine.Vector as Vector exposing (Vector)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Timing exposing (Timing)


rotation : Float -> Controls -> Rigidbody -> Rigidbody
rotation dt controls rigidbody =
    rigidbody
        |> Rigidbody.rotate (Quaternion.xRotation (controls.rudderPitch * dt * 0.1 |> degrees))
        |> Rigidbody.rotate (Quaternion.yRotation (controls.rudderYaw * dt * 0.1 |> degrees))


movement : Float -> Controls -> Rigidbody -> Rigidbody
movement dt controls rigidbody =
    rigidbody
        |> Rigidbody.translateRelative (Vector.back |> Vector.scale -(controls.throttle * controls.enginePower * dt))



-- MODULE


type Module
    = RudderPitchInput
    | RudderYawInput
    | ThrottleButtons
    | StateDump
    | RigidBodyDebug
    | QuaternionDump
    | OrientationDisplay



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
        (Rigidbody.new Vector.zero 2000)
        [ RudderPitchInput
        , RudderYawInput
        , ThrottleButtons
        , StateDump
        , RigidBodyDebug
        , QuaternionDump
        , OrientationDisplay
        ]
        (Controls.new 1 0.1)
        0
        (Dict.fromList
            [ ( "w", ( SteeringPitchInput 1, SteeringPitchInput 0 ) )
            , ( "s", ( SteeringPitchInput -1, SteeringPitchInput 0 ) )
            , ( "a", ( SteeringYawInput -1, SteeringYawInput 0 ) )
            , ( "d", ( SteeringYawInput 1, SteeringYawInput 0 ) )
            ]
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | SteeringPitchInput Float
    | SteeringYawInput Float
    | ThrottleInput Float


physicsUpdate : Controls -> Float -> Rigidbody -> Rigidbody
physicsUpdate controls dt rigidbody =
    rigidbody
        |> Rigidbody.step dt
        |> rotation dt controls
        |> movement dt controls


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
              }
            , Cmd.none
            )

        SteeringPitchInput input ->
            ( { model | controls = Controls.setRudderPitch input model.controls }
            , Cmd.none
            )

        SteeringYawInput input ->
            ( { model | controls = Controls.setRudderYaw input model.controls }
            , Cmd.none
            )

        ThrottleInput input ->
            ( { model | controls = Controls.setThrottle input model.controls }
            , Cmd.none
            )



-- VIEW
-- viewVector : List (Svg.Attribute msg) -> Vector -> Svg msg
-- viewVector attrs vector =
--     let
--         to : Vector
--         to =
--             Vector.scale 50 vector
--     in
--     Svg.line
--         ([ Svg.Attributes.x1 "0"
--          , Svg.Attributes.y1 "0"
--          , Svg.Attributes.x2 (String.fromFloat to.x)
--          , Svg.Attributes.y2 (String.fromFloat to.y)
--          ]
--             ++ attrs
--         )
--         []


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
            100
    in
    Svg.g
        [ Svg.Attributes.stroke "#262626"
        , Svg.Attributes.transform
            ("translate("
                ++ String.fromInt -(modBy spacing (round pos.x))
                ++ ", "
                ++ String.fromInt -(modBy spacing (round pos.z))
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


viewPhysicsDebug : Rigidbody -> Html msg
viewPhysicsDebug rigidbody =
    Html.div []
        [ Svg.svg
            [ Svg.Attributes.viewBox "-250 -250 500 500"
            , Svg.Attributes.class "movement-debug"
            ]
            [ viewGrid rigidbody.position
            , Svg.g
                [ Svg.Attributes.strokeWidth "7"
                , Svg.Attributes.transform
                    ("rotate("
                        ++ String.fromFloat ((Quaternion.yToEuler rigidbody.orientation * -180) / pi)
                        ++ ")"
                    )
                ]
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
                    , Svg.Attributes.y2 "100"
                    , Svg.Attributes.stroke "blue"
                    ]
                    []
                ]

            -- , Svg.g
            --     [ Svg.Attributes.strokeWidth "7"
            --     , Svg.Attributes.stroke "white"
            --     , Svg.Attributes.strokeLinecap "round"
            --     ]
            --     [ viewVector
            --         [ Svg.Attributes.stroke "orange" ]
            --         (Rigidbody.velocity rigidbody)
            --     , Svg.g [ Svg.Attributes.transform ("rotate(" ++ String.fromFloat (Quaternion.zToEuler rigidbody.orientation * 180 / pi) ++ ")") ]
            --         [ Svg.line
            --             [ Svg.Attributes.x1 "0"
            --             , Svg.Attributes.y1 "0"
            --             , Svg.Attributes.x2 "100"
            --             , Svg.Attributes.y2 "0"
            --             , Svg.Attributes.stroke "red"
            --             ]
            --             []
            --         , Svg.line
            --             [ Svg.Attributes.x1 "0"
            --             , Svg.Attributes.y1 "0"
            --             , Svg.Attributes.x2 "0"
            --             , Svg.Attributes.y2 "-100"
            --             , Svg.Attributes.stroke "green"
            --             ]
            --             []
            --         , Svg.line
            --             [ Svg.Attributes.x1 "0"
            --             , Svg.Attributes.y1 "0"
            --             , Svg.Attributes.x2 "0"
            --             , Svg.Attributes.y2 "0"
            --             , Svg.Attributes.stroke "blue"
            --             ]
            --             []
            --         ]
            --     ]
            ]
        ]


viewRudderButtons : String -> (Float -> Msg) -> Controls -> Html Msg
viewRudderButtons axisName inputMsg _ =
    Html.div []
        [ Html.h1 [] [ Html.text axisName ]
        , Html.div [ Html.Attributes.class "button-group" ]
            [ Html.button
                [ Html.Events.onMouseDown (inputMsg -1)
                , Html.Events.onMouseUp (inputMsg 0)
                ]
                [ Html.text "-" ]
            , Html.button
                [ Html.Events.onMouseDown (inputMsg 1)
                , Html.Events.onMouseUp (inputMsg 0)
                ]
                [ Html.text "+" ]
            ]

        -- , Html.input
        --     [ Html.Attributes.type_ "range"
        --     , Html.Attributes.min "-1"
        --     , Html.Attributes.max "1"
        --     , Html.Attributes.step "0.1"
        --     , Html.Attributes.value (String.fromFloat controls.rudderPitch)
        --     , Html.Events.onInput (String.toFloat >> Maybe.withDefault controls.rudderPitch >> inputMsg)
        --     , Html.Events.onMouseUp (inputMsg 0)
        --     ]
        --     []
        -- , Html.meter
        --     [ Html.Attributes.min "-1"
        --     , Html.Attributes.max "1"
        --     , Html.Attributes.value (String.fromFloat controls.rudderPitch)
        --     ]
        --     []
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
            , Html.Attributes.value (String.fromFloat controls.throttle)
            , Html.Events.onInput (String.toFloat >> Maybe.withDefault controls.throttle >> ThrottleInput)
            , Html.Events.onMouseUp (ThrottleInput 0)
            ]
            []

        -- , Html.meter
        --     [ Html.Attributes.min "-1"
        --     , Html.Attributes.max "1"
        --     , Html.Attributes.value (String.fromFloat controls.throttle)
        --     ]
        --     []
        ]


radToDegrees : Float -> Float
radToDegrees rad =
    rad * 180 / pi


viewStateDump : Rigidbody -> Html msg
viewStateDump rigidbody =
    Html.div []
        [ Html.p []
            [ Html.text "Pitch: "
            , Html.text (rigidbody.orientation |> Quaternion.xToEuler |> radToDegrees |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Yaw: "
            , Html.text (rigidbody.orientation |> Quaternion.yToEuler |> radToDegrees |> prettyFloat)
            ]
        , Html.p []
            [ Html.text "Roll: "
            , Html.text (rigidbody.orientation |> Quaternion.zToEuler |> radToDegrees |> prettyFloat)
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


viewQuaternionDump : Quaternion -> Html msg
viewQuaternionDump quaternion =
    Html.div []
        [ Html.p [] [ Html.text "scalar: ", Html.text (prettyFloat quaternion.scalar) ]
        , Html.p [] [ Html.text "x: ", Html.text (prettyFloat quaternion.vector.x) ]
        , Html.p [] [ Html.text "y: ", Html.text (prettyFloat quaternion.vector.y) ]
        , Html.p [] [ Html.text "z: ", Html.text (prettyFloat quaternion.vector.z) ]
        , Html.p [] [ Html.text "mag: ", Html.text (String.fromFloat (Quaternion.magnitude quaternion)) ]
        ]


viewOrientationDisplay : Rigidbody -> Svg msg
viewOrientationDisplay rigidbody =
    let
        pitch =
            rigidbody.orientation
                |> Quaternion.xToEuler
                |> radToDegrees

        roll =
            rigidbody.orientation
                |> Quaternion.zToEuler
                |> radToDegrees

        viewHorizontalLine i =
            Svg.g [ Svg.Attributes.transform ("translate(0, " ++ ((pitch * 3) - i * 3 |> String.fromFloat) ++ ")") ]
                [ Svg.line
                    [ Svg.Attributes.x1 "-250"
                    , Svg.Attributes.x2 "250"
                    , Svg.Attributes.y1 "0"
                    , Svg.Attributes.y2 "0"
                    , Svg.Attributes.stroke
                        (if i > 0 then
                            "blue"

                         else if i < 0 then
                            "orange"

                         else
                            "black"
                        )
                    ]
                    []
                , Svg.text_
                    [ Svg.Attributes.fontSize "1.5rem"
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.y "-5"
                    ]
                    [ Svg.text (String.fromFloat i) ]
                ]
    in
    Html.div []
        [ Svg.svg
            [ Svg.Attributes.viewBox "-250 -250 500 500"
            , Svg.Attributes.class "orientation-display"
            ]
            [ Svg.g [ Svg.Attributes.transform ("rotate(" ++ String.fromFloat roll ++ ")") ]
                (List.range -6 6 |> List.map (\n -> toFloat (n * 30)) |> List.map viewHorizontalLine)
            , Svg.line
                [ Svg.Attributes.x1 "-250"
                , Svg.Attributes.x2 "250"
                , Svg.Attributes.y1 "0"
                , Svg.Attributes.y2 "0"
                , Svg.Attributes.stroke "black"
                ]
                []
            ]
        ]


view : Model -> Html Msg
view model =
    let
        viewModule : Module -> Html Msg
        viewModule m =
            case m of
                RudderPitchInput ->
                    viewRudderButtons "Pitch" SteeringPitchInput model.controls

                RudderYawInput ->
                    viewRudderButtons "Yaw" SteeringYawInput model.controls

                ThrottleButtons ->
                    viewThrottleButtons model.controls

                StateDump ->
                    viewStateDump model.particle

                RigidBodyDebug ->
                    viewPhysicsDebug model.particle

                QuaternionDump ->
                    viewQuaternionDump model.particle.orientation

                OrientationDisplay ->
                    viewOrientationDisplay model.particle
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
