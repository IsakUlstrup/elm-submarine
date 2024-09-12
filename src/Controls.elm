module Controls exposing
    ( Controls
    , new
    , setRudderInput
    , setThrottleInput
    , tick
    )


type alias Controls =
    { throttle : Float
    , rudder : Float
    , rudderInput : Float
    , throttleInput : Float
    , rudderSize : Float
    , enginePower : Float
    }


new : Float -> Float -> Controls
new rudderSize enginePower =
    Controls 0 0 0 0 rudderSize enginePower


stop : Float -> Float
stop n =
    if abs n < 0.01 then
        0

    else
        n


tick : Float -> Controls -> Controls
tick dt submarine =
    { submarine
        | throttle =
            submarine.throttle
                + ((submarine.throttleInput - submarine.throttle) * dt * 0.002)
                |> clamp -1 1
                |> stop
        , rudder =
            submarine.rudder
                + ((submarine.rudderInput - submarine.rudder) * dt * 0.0015)
                |> clamp -1 1
                |> stop
    }


setThrottleInput : Float -> Controls -> Controls
setThrottleInput throttle submarine =
    { submarine | throttleInput = throttle |> clamp -1 1 }


setRudderInput : Float -> Controls -> Controls
setRudderInput r submarine =
    { submarine | rudderInput = r |> clamp -1 1 }
