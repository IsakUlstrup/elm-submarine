module Main exposing (Model, Msg, main)

import Array
import Browser
import Browser.Events
import Dict
import Engine.Module exposing (Module, Modules, Signal(..))
import Engine.Particle as Particle
import Engine.Vector2 as Vector2
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Submarine exposing (Submarine)



-- PART


type Part
    = Button Bool
    | ToggleButton Bool
    | Slider Float
    | SteeringController String Float
    | KeyboardToggle String Bool


buttonSteeringModule : Module Part
buttonSteeringModule =
    Engine.Module.newModule
        [ ( "SteeringSlider", Slider 0 )
        , ( "Steering", SteeringController "SteeringSlider" 0 )
        , ( "Button", Button False )
        ]


keyboardInputModule : Module Part
keyboardInputModule =
    Engine.Module.newModule
        [ ( "Left", KeyboardToggle "a" False )
        , ( "Right", KeyboardToggle "d" False )
        , ( "Toggle", ToggleButton False )
        ]



-- -- MODULE
-- type Module
--     = InputButtons ButtonState
--     | KeyboardInput ButtonState
--     | ControlsState
--     | StateDump
--     | Compass
--     | Movement
-- type alias ButtonState =
--     { left : Bool
--     , forwards : Bool
--     , right : Bool
--     }
-- type Direction
--     = Left
--     | Forwards
--     | Right
-- updateButtonState : (ButtonState -> ButtonState) -> Module -> Module
-- updateButtonState f mod =
--     case mod of
--         InputButtons state ->
--             InputButtons (f state)
--         _ ->
--             mod
-- updateKeyboardInputState : (ButtonState -> ButtonState) -> Module -> Module
-- updateKeyboardInputState f mod =
--     case mod of
--         KeyboardInput state ->
--             KeyboardInput (f state)
--         _ ->
--             mod
-- handleKeyPress : String -> Bool -> ButtonState -> ButtonState
-- handleKeyPress key pressed state =
--     case key of
--         "a" ->
--             setDirectionPressed Left pressed state
--         "d" ->
--             setDirectionPressed Right pressed state
--         "w" ->
--             setDirectionPressed Forwards pressed state
--         _ ->
--             state
-- setDirectionPressed : Direction -> Bool -> ButtonState -> ButtonState
-- setDirectionPressed direction pressed state =
--     case direction of
--         Left ->
--             { state | left = pressed }
--         Forwards ->
--             { state | forwards = pressed }
--         Right ->
--             { state | right = pressed }
-- MODEL


type alias Model =
    { submarine : Submarine

    -- , slots : Array (Maybe Module)
    , modules : Modules Part
    }



-- updateSlot : Int -> (Module -> Module) -> Model -> Model
-- updateSlot index f model =
--     let
--         slot =
--             Array.get index model.slots
--                 |> Maybe.andThen
--                     (Maybe.map f)
--     in
--     { model | slots = Array.set index slot model.slots }
-- updateSlots : (Module -> Module) -> Model -> Model
-- updateSlots f model =
--     { model | slots = Array.map (Maybe.map f) model.slots }
-- applyModules : Model -> Model
-- applyModules model =
--     let
--         modules =
--             model.slots |> Array.toList |> List.filterMap identity
--         applyModule m s =
--             case m of
--                 InputButtons state ->
--                     if state.left then
--                         s |> Particle.updateState (Submarine.setRudderInput -1)
--                     else if state.right then
--                         s |> Particle.updateState (Submarine.setRudderInput 1)
--                     else if state.forwards then
--                         s |> Particle.updateState (Submarine.setThrottleInput 1)
--                     else
--                         s
--                             |> Particle.updateState (Submarine.setThrottleInput 0)
--                             |> Particle.updateState (Submarine.setRudderInput 0)
--                 KeyboardInput state ->
--                     if state.left then
--                         s |> Particle.updateState (Submarine.setRudderInput -1)
--                     else if state.right then
--                         s |> Particle.updateState (Submarine.setRudderInput 1)
--                     else if state.forwards then
--                         s |> Particle.updateState (Submarine.setThrottleInput 1)
--                     else
--                         s
--                             |> Particle.updateState (Submarine.setThrottleInput 0)
--                             |> Particle.updateState (Submarine.setRudderInput 0)
--                 _ ->
--                     s
--     in
--     { model | submarine = List.foldl applyModule model.submarine modules }


applyPart : Part -> Submarine -> Submarine
applyPart part submarine =
    case part of
        SteeringController _ value ->
            submarine
                |> Particle.updateState (Submarine.setRudderInput value)

        _ ->
            submarine


applyModule : Module Part -> Submarine -> Submarine
applyModule m submarine =
    List.foldl applyPart submarine (Dict.toList m |> List.map Tuple.second)


applyModules : Modules Part -> Submarine -> Submarine
applyModules modules submarine =
    List.foldl applyModule submarine (Array.toList modules)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Particle.new Submarine.new Vector2.zero 100)
        -- (Array.repeat 8 Nothing)
        (Engine.Module.empty
            |> Engine.Module.addModule buttonSteeringModule
            |> Engine.Module.addModule keyboardInputModule
            |> Engine.Module.addModule buttonSteeringModule
            |> Engine.Module.addModule keyboardInputModule
        )
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | ModuleMsg Int Engine.Module.ModuleMsg



-- | ClickedAddModule Int Module
-- | ClickedInputButton Int Direction Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | submarine =
                    model.submarine
                        |> applyModules model.modules
                        |> Submarine.stepParticle dt
                        |> Submarine.controlsUpdate dt
                        |> Submarine.applyThrust
                        |> Submarine.rudderForce
                        |> Submarine.applyRotation
                        |> Submarine.friction
              }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | modules = Engine.Module.updateModules updatePart { name = key, signal = Bool True } model.modules }
            , Cmd.none
            )

        KeyUp key ->
            ( { model | modules = Engine.Module.updateModules updatePart { name = key, signal = Bool False } model.modules }
            , Cmd.none
            )

        ModuleMsg index moduleMsg ->
            ( { model | modules = Engine.Module.updateModuleAtIndex index updatePart moduleMsg model.modules }
            , Cmd.none
            )


updatePart : Engine.Module.ModuleMsg -> String -> Part -> Part
updatePart moduleMsg name part =
    case ( part, moduleMsg.signal ) of
        ( SteeringController inputSignal signal, Float value ) ->
            (if moduleMsg.name == inputSignal then
                value

             else
                signal
            )
                |> SteeringController inputSignal

        ( Slider _, Float float ) ->
            if name == moduleMsg.name then
                Slider float

            else
                part

        ( KeyboardToggle inputSignal _, Bool bool ) ->
            if moduleMsg.name == inputSignal then
                KeyboardToggle inputSignal bool

            else
                part

        ( Button _, Bool bool ) ->
            if name == moduleMsg.name then
                Button bool

            else
                part

        ( ToggleButton _, Bool bool ) ->
            if name == moduleMsg.name then
                ToggleButton bool

            else
                part

        _ ->
            part



-- ClickedAddModule index mod ->
--     ( { model | slots = Array.set index (Just mod) model.slots }
--     , Cmd.none
--     )
-- ClickedInputButton index direction pressed ->
--     ( updateSlot index (updateButtonState <| setDirectionPressed direction pressed) model
--     , Cmd.none
--     )
-- VIEW
-- prettyFloat : Float -> String
-- prettyFloat f =
--     case f |> String.fromFloat |> String.split "." of
--         [ x ] ->
--             x
--         x :: xs ->
--             x ++ "." ++ String.left 1 (String.concat xs)
--         [] ->
--             "Error"
-- viewVector : List (Svg.Attribute msg) -> Vector2 -> Svg msg
-- viewVector attrs vector =
--     let
--         ( x2, y2 ) =
--             ( vector.x * 50, vector.y * 50 )
--     in
--     Svg.line
--         ([ Svg.Attributes.x1 "0"
--          , Svg.Attributes.y1 "0"
--          , Svg.Attributes.x2 (String.fromFloat x2)
--          , Svg.Attributes.y2 (String.fromFloat y2)
--          ]
--             ++ attrs
--         )
--         []
-- viewSubmarine : Submarine -> Svg msg
-- viewSubmarine submarine =
--     Svg.g
--         []
--         [ Svg.circle
--             [ Svg.Attributes.r (String.fromFloat submarine.radius)
--             , Svg.Attributes.fill "white"
--             ]
--             []
--         , Svg.g
--             [ Svg.Attributes.strokeWidth "7"
--             , Svg.Attributes.strokeLinecap "round"
--             ]
--             [ viewVector
--                 [ Svg.Attributes.stroke "red" ]
--                 submarine.orientation
--             , viewVector
--                 [ Svg.Attributes.stroke "green" ]
--                 (Vector2.orthogonal submarine.orientation |> Vector2.scale -submarine.state.rudder)
--             , viewVector
--                 [ Svg.Attributes.stroke "orange" ]
--                 (Particle.velocity submarine)
--             , viewVector
--                 [ Svg.Attributes.stroke "cyan" ]
--                 (submarine.orientation |> Vector2.scale -1 |> Vector2.rotate -submarine.state.rudder)
--             ]
--         ]
-- viewGrid : Vector2 -> Svg msg
-- viewGrid pos =
--     let
--         verticalLine x =
--             Svg.line
--                 [ Svg.Attributes.x1 (String.fromInt x)
--                 , Svg.Attributes.y1 "-500"
--                 , Svg.Attributes.x2 (String.fromInt x)
--                 , Svg.Attributes.y2 "500"
--                 ]
--                 []
--         horizontalLine y =
--             Svg.line
--                 [ Svg.Attributes.x1 "-500"
--                 , Svg.Attributes.y1 (String.fromInt y)
--                 , Svg.Attributes.x2 "500"
--                 , Svg.Attributes.y2 (String.fromInt y)
--                 ]
--                 []
--         spacing =
--             200
--     in
--     Svg.g
--         [ Svg.Attributes.stroke "#262626"
--         , Svg.Attributes.transform
--             ("translate("
--                 ++ String.fromInt -(modBy spacing (round pos.x))
--                 ++ ", "
--                 ++ String.fromInt -(modBy spacing (round pos.y))
--                 ++ ")"
--             )
--         ]
--         (List.range -2 2
--             |> List.concatMap
--                 (\i ->
--                     [ verticalLine (i * spacing)
--                     , horizontalLine (i * spacing)
--                     ]
--                 )
--         )
-- viewMovementDebug : Submarine -> Html msg
-- viewMovementDebug submarine =
--     Svg.svg
--         [ Svg.Attributes.viewBox "-250 -250 500 500"
--         ]
--         [ viewGrid submarine.position
--         , viewSubmarine submarine
--         ]
-- viewInputButtons : Int -> ButtonState -> Html Msg
-- viewInputButtons index state =
--     let
--         pointerMsg direction pressed =
--             Decode.succeed (ClickedInputButton index direction pressed)
--     in
--     Html.div [ Svg.Attributes.class "module" ]
--         [ Html.button
--             [ Html.Events.on "pointerdown" (pointerMsg Left True)
--             , Html.Events.on "pointerup" (pointerMsg Left False)
--             , Html.Events.on "pointerleave" (pointerMsg Left False)
--             , Html.Attributes.classList [ ( "down", state.left ) ]
--             ]
--             [ Html.text "Port" ]
--         , Html.button
--             [ Html.Events.on "pointerdown" (pointerMsg Forwards True)
--             , Html.Events.on "pointerup" (pointerMsg Forwards False)
--             , Html.Events.on "pointerleave" (pointerMsg Forwards False)
--             , Html.Attributes.classList [ ( "down", state.forwards ) ]
--             ]
--             [ Html.text "Forwards" ]
--         , Html.button
--             [ Html.Events.on "pointerdown" (pointerMsg Right True)
--             , Html.Events.on "pointerup" (pointerMsg Right False)
--             , Html.Events.on "pointerleave" (pointerMsg Right False)
--             , Html.Attributes.classList [ ( "down", state.right ) ]
--             ]
--             [ Html.text "Starboard" ]
--         ]
-- viewKeyboardInput : ButtonState -> Html Msg
-- viewKeyboardInput state =
--     Html.div [ Svg.Attributes.class "module" ]
--         [ Html.button
--             [ Html.Attributes.classList [ ( "down", state.left ) ]
--             ]
--             [ Html.text "Port" ]
--         , Html.button
--             [ Html.Attributes.classList [ ( "down", state.forwards ) ]
--             ]
--             [ Html.text "Forwards" ]
--         , Html.button
--             [ Html.Attributes.classList [ ( "down", state.right ) ]
--             ]
--             [ Html.text "Starboard" ]
--         ]
-- viewControlsState : Submarine -> Html Msg
-- viewControlsState submarine =
--     Html.div [ Html.Attributes.class "module" ]
--         [ Html.h1 [] [ Html.text "Rudder" ]
--         , Html.div
--             [ Html.Attributes.style "display" "flex"
--             , Html.Attributes.style "width" "100%"
--             , Html.Attributes.style "gap" "0.5rem"
--             ]
--             [ Html.meter
--                 [ Html.Attributes.value (String.fromFloat (submarine.state.rudder |> min 0 |> abs))
--                 , Html.Attributes.style "transform" "rotate(180deg)"
--                 ]
--                 []
--             , Html.meter
--                 [ Html.Attributes.value (String.fromFloat submarine.state.rudder)
--                 ]
--                 []
--             ]
--         , Html.h1 [] [ Html.text "Throttle" ]
--         , Html.meter
--             [ Html.Attributes.value (String.fromFloat submarine.state.throttle)
--             ]
--             []
--         ]
-- viewCompass : Submarine -> Html Msg
-- viewCompass submarine =
--     let
--         ( x2, y2 ) =
--             ( submarine.orientation.x * 35, submarine.orientation.y * 35 )
--     in
--     Html.div [ Svg.Attributes.class "module" ]
--         [ Svg.svg
--             [ Svg.Attributes.viewBox "-50 -50 100 100"
--             ]
--             [ Svg.circle [ Svg.Attributes.r "50", Svg.Attributes.fill "white" ] []
--             , Svg.g
--                 [ Svg.Attributes.dominantBaseline "central"
--                 , Svg.Attributes.textAnchor "middle"
--                 , Svg.Attributes.fontSize "0.5rem"
--                 ]
--                 [ Svg.text_
--                     [ Svg.Attributes.y "-40" ]
--                     [ Svg.text "N" ]
--                 , Svg.text_
--                     [ Svg.Attributes.y "40" ]
--                     [ Svg.text "S" ]
--                 , Svg.text_
--                     [ Svg.Attributes.x "-40" ]
--                     [ Svg.text "W" ]
--                 , Svg.text_
--                     [ Svg.Attributes.x "40" ]
--                     [ Svg.text "E" ]
--                 ]
--             , Svg.line
--                 [ Svg.Attributes.x1 "0"
--                 , Svg.Attributes.y1 "0"
--                 , Svg.Attributes.x2 (String.fromFloat x2)
--                 , Svg.Attributes.y2 (String.fromFloat y2)
--                 , Svg.Attributes.stroke "red"
--                 , Svg.Attributes.strokeWidth "2"
--                 , Svg.Attributes.strokeLinecap "round"
--                 ]
--                 []
--             ]
--         ]
-- viewStateDump : Submarine -> Html msg
-- viewStateDump submarine =
--     Html.div [ Html.Attributes.class "module" ]
--         [ Html.p []
--             [ Html.text "Position: "
--             , Html.text (prettyFloat submarine.position.x)
--             , Html.text ", "
--             , Html.text (prettyFloat submarine.position.y)
--             ]
--         , Html.p []
--             [ Html.text "Rotation (deg): "
--             , Html.text (prettyFloat (Vector2.angleDegrees submarine.orientation))
--             ]
--         , Html.p []
--             [ Html.text "Rotation (rad): "
--             , Html.text (prettyFloat (Vector2.angleRadian submarine.orientation))
--             ]
--         , Html.p []
--             [ Html.text "Velocity: "
--             , Html.text (submarine |> Particle.velocity |> Vector2.magnitude |> prettyFloat)
--             ]
--         , Html.p []
--             [ Html.text "Throttle: "
--             , Html.text (submarine.state.throttle |> prettyFloat)
--             ]
--         , Html.p []
--             [ Html.text "Rudder: "
--             , Html.text (submarine.state.rudder |> prettyFloat)
--             ]
--         ]
-- viewMovement : Submarine -> Html msg
-- viewMovement submarine =
--     Html.div [ Svg.Attributes.class "module", Svg.Attributes.class "fill" ]
--         [ Svg.Lazy.lazy viewMovementDebug submarine
--         ]
-- viewModule : Int -> Submarine -> Module -> Html Msg
-- viewModule index submarine m =
--     case m of
--         InputButtons state ->
--             viewInputButtons index state
--         KeyboardInput state ->
--             viewKeyboardInput state
--         ControlsState ->
--             viewControlsState submarine
--         StateDump ->
--             viewStateDump submarine
--         Compass ->
--             viewCompass submarine
--         Movement ->
--             viewMovement submarine
-- viewSlot : Submarine -> ( Int, Maybe Module ) -> Html Msg
-- viewSlot submarine ( index, slot ) =
--     Html.div [ Html.Attributes.class "slot" ]
--         (case slot of
--             Just m ->
--                 [ viewModule index submarine m ]
--             Nothing ->
--                 [ Html.button [ Html.Attributes.attribute "popovertarget" ("add-module-" ++ String.fromInt index) ] [ Html.text "Install module" ]
--                 , Html.div
--                     [ Html.Attributes.attribute "popover" ""
--                     , Html.Attributes.id ("add-module-" ++ String.fromInt index)
--                     , Html.Attributes.class "add-module-popup"
--                     ]
--                     [ Html.h3 [] [ Html.text ("Slot #" ++ String.fromInt index) ]
--                     , Html.ul [ Html.Attributes.class "modules" ]
--                         [ Html.li [ Html.Events.onClick (ClickedAddModule index (InputButtons { left = False, forwards = False, right = False })) ] [ Html.text "Input buttons" ]
--                         , Html.li [ Html.Events.onClick (ClickedAddModule index (KeyboardInput { left = False, forwards = False, right = False })) ] [ Html.text "Keyboard input" ]
--                         , Html.li [ Html.Events.onClick (ClickedAddModule index ControlsState) ] [ Html.text "Controls state" ]
--                         , Html.li [ Html.Events.onClick (ClickedAddModule index StateDump) ] [ Html.text "State view" ]
--                         , Html.li [ Html.Events.onClick (ClickedAddModule index Compass) ] [ Html.text "Compass" ]
--                         , Html.li [ Html.Events.onClick (ClickedAddModule index Movement) ] [ Html.text "Movement" ]
--                         ]
--                     ]
--                 ]
--         )


viewPart : ( String, Part ) -> Html Engine.Module.ModuleMsg
viewPart ( name, part ) =
    let
        msg s =
            { name = name
            , signal = s
            }
    in
    case part of
        Button pressed ->
            Html.button
                [ Html.Events.onMouseDown
                    (msg (Bool True))
                , Html.Events.onMouseUp
                    (msg (Bool False))
                , Html.Attributes.classList [ ( "down", pressed ) ]
                ]
                [ Html.text name ]

        ToggleButton pressed ->
            Html.button
                [ Html.Events.onMouseDown
                    (msg (Bool (not pressed)))
                , Html.Attributes.classList [ ( "down", pressed ) ]
                ]
                [ Html.text name ]

        Slider value ->
            Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "-1"
                , Html.Attributes.max "1"
                , Html.Attributes.step "0.1"
                , Html.Attributes.value (String.fromFloat value)
                , Html.Events.onInput
                    (String.toFloat
                        >> Maybe.withDefault value
                        >> Float
                        >> msg
                    )
                ]
                []

        SteeringController _ value ->
            Html.div [] [ Html.p [] [ Html.text "Rudder input" ], Html.text (value |> String.fromFloat) ]

        KeyboardToggle _ pressed ->
            Html.div []
                [ Html.text name
                , Html.text
                    (if pressed then
                        ": Down"

                     else
                        ": Up"
                    )
                ]


viewModule : ( Int, Module Part ) -> Html Msg
viewModule ( index, m ) =
    Html.div [ Html.Attributes.class "module" ]
        (m
            |> Dict.toList
            |> List.map viewPart
        )
        |> Html.map (ModuleMsg index)


view : Model -> Html Msg
view model =
    main_ [ Html.Attributes.id "app" ]
        -- (model.slots
        --     |> Array.toIndexedList
        --     |> List.map (viewSlot model.submarine)
        -- )
        (model.modules
            |> Engine.Module.modulesToList
            |> List.map viewModule
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
        [ Browser.Events.onKeyDown (keyDecoder KeyDown)
        , Browser.Events.onKeyUp (keyDecoder KeyUp)
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
