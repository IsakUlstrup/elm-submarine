module Main exposing (Model, Module, Msg, main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Engine.Particle as Particle
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Html.Lazy
import Json.Decode as Decode exposing (Decoder)
import Submarine exposing (Submarine)
import Svg exposing (Svg)
import Svg.Attributes



-- MODULE


type Module
    = SteeringButtons
    | ThrottleButtons
    | InputState
    | PhysicsDebug



-- MODEL


type alias Model =
    { submarine : Submarine
    , modules : List Module
    , keybinds : Dict String ( Msg, Msg )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Particle.new Submarine.new Vector2.zero 100)
        [ SteeringButtons
        , ThrottleButtons
        , InputState
        , PhysicsDebug
        ]
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
            , Cmd.none
            )

        SteeringInput input ->
            ( { model | submarine = Particle.updateState (Submarine.setRudderInput input) model.submarine }
            , Cmd.none
            )

        ThrottleInput input ->
            ( { model | submarine = Particle.updateState (Submarine.setThrottleInput input) model.submarine }
            , Cmd.none
            )



-- VIEW


viewVector : List (Svg.Attribute msg) -> Vector2 -> Svg msg
viewVector attrs vector =
    let
        to =
            Vector2.scale 50 vector
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


viewSubmarine : Submarine -> Svg msg
viewSubmarine submarine =
    Svg.g
        [ Svg.Attributes.strokeWidth "7"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeLinecap "round"
        ]
        [ viewVector
            [ Svg.Attributes.stroke "red" ]
            (submarine |> Particle.forwards)
        , viewVector
            [ Svg.Attributes.stroke "orange" ]
            (Particle.velocity submarine)
        , viewVector
            [ Svg.Attributes.stroke "cyan" ]
            (submarine |> Particle.forwards |> Vector2.scale -1 |> Vector2.rotate submarine.state.rudder)
        ]


viewGrid : Vector2 -> Svg msg
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
            x

        x :: xs ->
            x ++ "." ++ (xs |> String.concat |> String.left 2)

        [] ->
            "E: " ++ String.fromFloat n


viewPhysicsDebug : Submarine -> Html msg
viewPhysicsDebug submarine =
    Html.div []
        [ Svg.svg
            [ Svg.Attributes.viewBox "-250 -250 500 500"
            , Svg.Attributes.class "movement-debug"

            -- flip svg y axis so we can use cartesian coordinates
            , Svg.Attributes.transform "matrix(1 0 0 -1 0 0)"
            ]
            [ viewGrid submarine.position
            , viewSubmarine submarine
            ]
        , Html.div []
            [ Html.p []
                [ Html.text "Velocity: "
                , Html.text (submarine |> Particle.velocity |> Vector2.magnitude |> prettyFloat)
                ]
            ]
        ]


viewInputState : Submarine -> Html msg
viewInputState submarine =
    Html.div []
        [ Html.h1 [] [ Html.text "Rudder" ]
        , Html.div []
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
        , Html.div []
            [ Html.meter
                [ Html.Attributes.value (String.fromFloat (submarine.state.throttle |> min 0 |> abs))
                , Html.Attributes.style "transform" "rotate(180deg)"
                ]
                []
            , Html.meter
                [ Html.Attributes.value (String.fromFloat submarine.state.throttle)
                ]
                []
            ]
        ]


viewSteeringButtons : Html Msg
viewSteeringButtons =
    Html.div []
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


viewThrottleButtons : Html Msg
viewThrottleButtons =
    Html.div []
        [ Html.button
            [ Html.Events.onMouseDown (ThrottleInput 1)
            , Html.Events.onMouseUp (ThrottleInput 0)
            ]
            [ Html.text "Forward" ]
        , Html.button
            [ Html.Events.onMouseDown (ThrottleInput -1)
            , Html.Events.onMouseUp (ThrottleInput 0)
            ]
            [ Html.text "Reverse" ]
        ]


view : Model -> Html Msg
view model =
    let
        viewModule : Module -> Html Msg
        viewModule m =
            Html.div [ Html.Attributes.class "module" ]
                [ case m of
                    SteeringButtons ->
                        viewSteeringButtons

                    ThrottleButtons ->
                        viewThrottleButtons

                    InputState ->
                        viewInputState model.submarine

                    PhysicsDebug ->
                        Html.Lazy.lazy viewPhysicsDebug model.submarine
                ]
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
        , Browser.Events.onAnimationFrameDelta (min 50 >> Tick)
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
