module SubmarineState exposing (SubmarineState, new, setRudderInput, setThrottleInput, tickControls)

-- SUBMARINE


type alias SubmarineState =
    { throttle : Float
    , rudder : Float
    , rudderInput : Float
    , throttleInput : Float
    }


new : SubmarineState
new =
    SubmarineState 0 0 0 0


tickControls : Float -> SubmarineState -> SubmarineState
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


setThrottleInput : Float -> SubmarineState -> SubmarineState
setThrottleInput throttle submarine =
    { submarine | throttleInput = throttle |> clamp -1 1 }


setRudderInput : Float -> SubmarineState -> SubmarineState
setRudderInput r submarine =
    { submarine | rudderInput = r |> clamp -1 1 }
