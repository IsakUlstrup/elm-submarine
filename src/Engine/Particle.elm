module Engine.Particle exposing
    ( Particle
    , applyForce
    , applyRotationalForce
    , forwards
    , new
    , setMass
    , setOrientation
    , setPosition
    , step
    , updateState
    , velocity
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


{-| A particle meant to be used with Verlet integration
-}
type alias Particle a =
    { position : Vector2
    , oldPosition : Vector2
    , acceleration : Vector2
    , orientation : Float
    , rotationVelocity : Float
    , rotationAcceleration : Float
    , mass : Float
    , radius : Float
    , state : a
    }


{-| Particle constructor
-}
new : a -> Vector2 -> Float -> Particle a
new state position mass =
    Particle position position Vector2.zero 0 0 0 mass 50 state


updateState : (a -> a) -> Particle a -> Particle a
updateState f particle =
    { particle | state = f particle.state }


applyForce : Vector2 -> Particle a -> Particle a
applyForce force particle =
    if particle.mass /= 0 then
        { particle
            | acceleration =
                force
                    |> Vector2.divide particle.mass
                    |> Vector2.add particle.acceleration
        }

    else
        particle


applyRotationalForce : Float -> Particle a -> Particle a
applyRotationalForce force particle =
    if particle.mass /= 0 then
        { particle | rotationAcceleration = (force / particle.mass) + particle.rotationAcceleration }

    else
        particle


{-| Derive velocity vector based on old position
-}
velocity : Particle a -> Vector2
velocity particle =
    Vector2.subtract particle.oldPosition particle.position


setPosition : Vector2 -> Particle a -> Particle a
setPosition position particle =
    { particle | position = position, oldPosition = Vector2.subtract (velocity particle) position }


setOrientation : Float -> Particle a -> Particle a
setOrientation radian particle =
    { particle | orientation = radian }


setMass : Float -> Particle a -> Particle a
setMass mass particle =
    { particle | mass = max 0 mass }


forwards : Particle a -> Vector2
forwards particle =
    Vector2.new (sin particle.orientation) (cos particle.orientation)
        |> Vector2.normalize


{-| Step forwards using Verlet integration
-}
step : Float -> Particle a -> Particle a
step dt particle =
    { particle
        | position =
            particle.position
                |> Vector2.add (velocity particle)
                |> Vector2.add (Vector2.scale (dt ^ 2) particle.acceleration)
        , oldPosition = particle.position
        , acceleration = Vector2.zero
        , rotationVelocity = particle.rotationVelocity + (particle.rotationAcceleration * (dt ^ 2))
        , orientation = particle.orientation + particle.rotationVelocity
        , rotationAcceleration = 0
    }
