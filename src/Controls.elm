module Controls exposing
    ( Controls
    , new
    , setRudderPitch
    , setRudderYaw
    , setThrottle
    )


type alias Controls =
    { throttle : Float
    , rudderYaw : Float
    , rudderPitch : Float
    , rudderSize : Float
    , enginePower : Float
    }


new : Float -> Float -> Controls
new rudderSize enginePower =
    Controls 0 0 0 rudderSize enginePower



-- stop : Float -> Float
-- stop n =
--     if abs n < 0.01 then
--         0
--     else
--         n
-- tick : Float -> Controls -> Controls
-- tick dt submarine =
--     { submarine
--         | throttle =
--             submarine.throttle
--                 + ((submarine.throttleInput - submarine.throttle) * dt * 0.002)
--                 |> clamp -1 1
--                 |> stop
--         , rudder =
--             submarine.rudder
--                 + ((submarine.rudderInput - submarine.rudder) * dt * 0.0015)
--                 |> clamp -1 1
--                 |> stop
--     }


setThrottle : Float -> Controls -> Controls
setThrottle throttle submarine =
    { submarine | throttle = throttle |> clamp -1 1 }


setRudderYaw : Float -> Controls -> Controls
setRudderYaw r submarine =
    { submarine | rudderYaw = r |> clamp -1 1 }


setRudderPitch : Float -> Controls -> Controls
setRudderPitch r submarine =
    { submarine | rudderPitch = r |> clamp -1 1 }
