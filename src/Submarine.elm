module Submarine exposing (Submarine, SubmarineState, applyRotation, applyThrust, controlsUpdate, friction, new, rudderForce, setRudderInput, setThrottleInput, stepParticle, tickControls)

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)



-- SUBMARINE


type alias Submarine =
    Particle SubmarineState


applyThrust : Submarine -> Submarine
applyThrust submarine =
    let
        enginePower : Float
        enginePower =
            0.01

        force : Vector2
        force =
            submarine.orientation
                |> Vector2.scale (submarine.state.throttle * enginePower)
    in
    Particle.applyForce force submarine


rudderForce : Submarine -> Submarine
rudderForce submarine =
    let
        force =
            Vector2.orthogonal submarine.orientation
                |> Vector2.scale submarine.state.rudder
                |> Vector2.scale (Particle.velocity submarine |> Vector2.magnitude |> min 1)
                |> Vector2.scale 0.01

        frictionForce =
            force
                |> Vector2.scale
                    ((Particle.velocity submarine |> Vector2.magnitude) - Vector2.magnitude force)
                |> Vector2.scale -1
    in
    submarine
        |> Particle.applyForce force
        |> Particle.applyForce frictionForce


applyRotation : Submarine -> Submarine
applyRotation submarine =
    let
        steeringAuthority : Float
        steeringAuthority =
            0.001
    in
    Particle.applyRotationalForce
        ((submarine
            |> Particle.velocity
            |> Vector2.magnitude
         )
            * submarine.state.rudder
            * steeringAuthority
        )
        submarine


friction : Submarine -> Submarine
friction submarine =
    submarine
        |> Particle.applyRotationalForce -(submarine.rotationVelocity * 0.1)
        |> Particle.applyForce (Particle.velocity submarine |> Vector2.scale -0.002)


controlsUpdate : Float -> Submarine -> Submarine
controlsUpdate dt submarine =
    Particle.updateState (tickControls dt) submarine


stepParticle : Float -> Submarine -> Submarine
stepParticle dt submarine =
    Particle.step dt submarine



-- SUBMARINE STATE


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
            submarine.throttle
                + ((submarine.throttleInput - submarine.throttle) * dt * 0.002)
                |> clamp -1 1
        , rudder =
            submarine.rudder
                + ((submarine.rudderInput - submarine.rudder) * dt * 0.0015)
                |> clamp -1 1

        -- , rudderInput = 0
    }


setThrottleInput : Float -> SubmarineState -> SubmarineState
setThrottleInput throttle submarine =
    { submarine | throttleInput = throttle |> clamp -1 1 }


setRudderInput : Float -> SubmarineState -> SubmarineState
setRudderInput r submarine =
    { submarine | rudderInput = r |> clamp -1 1 }
