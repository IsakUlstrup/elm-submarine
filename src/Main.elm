module Main exposing (Model, Module, Msg, main)

import Browser
import Browser.Events
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


viewPhysicsDebug : Submarine -> Html msg
viewPhysicsDebug submarine =
    Svg.svg
        [ Svg.Attributes.viewBox "-250 -250 500 500"
        , Svg.Attributes.class "movement-debug"
        ]
        [ viewGrid submarine.position
        , viewSubmarine submarine
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


keyDecoder : Bool -> Decoder Msg
keyDecoder pressed =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "w" ->
                        if pressed then
                            Decode.succeed <| ThrottleInput 1

                        else
                            Decode.succeed <| ThrottleInput 0

                    "s" ->
                        if pressed then
                            Decode.succeed <| ThrottleInput -1

                        else
                            Decode.succeed <| ThrottleInput 0

                    "a" ->
                        if pressed then
                            Decode.succeed <| SteeringInput -1

                        else
                            Decode.succeed <| SteeringInput 0

                    "d" ->
                        if pressed then
                            Decode.succeed <| SteeringInput 1

                        else
                            Decode.succeed <| SteeringInput 0

                    _ ->
                        Decode.fail "unknown key"
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder True)
        , Browser.Events.onKeyUp (keyDecoder False)
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
