module Timing exposing (Timing, fixedUpdate)


type alias Timing =
    Float


{-| Step time constant in miliseconds
-}
stepTime : Float
stepTime =
    20


fixedUpdate : (Float -> a -> a) -> Float -> ( Timing, a ) -> ( Timing, a )
fixedUpdate f dt ( timing, state ) =
    if timing + dt >= stepTime then
        ( (timing + dt) - stepTime
        , f stepTime state
        )
            |> fixedUpdate f 0

    else
        ( timing + dt
        , state
        )



-- updateModel : (a -> a) -> a -> Timing -> ( Timing, a )
-- updateModel f state timing =
--     if timing.timeAccum >= stepTime then
--         let
--             newState =
--                 f state
--         in
--         { timing | timeAccum = timing.timeAccum - stepTime }
--             |> updateModel f newState
--     else
--         ( timing, state )
