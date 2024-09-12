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
    , velocity
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


{-| A particle meant to be used with Verlet integration
-}
type alias Particle =
    { position : Vector2
    , oldPosition : Vector2
    , acceleration : Vector2
    , orientation : Float
    , rotationVelocity : Float
    , rotationAcceleration : Float
    , mass : Float
    , radius : Float
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle
new position mass =
    Particle position position Vector2.zero 0 0 0 mass 50


applyForce : Vector2 -> Particle -> Particle
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


applyRotationalForce : Float -> Particle -> Particle
applyRotationalForce force particle =
    if particle.mass /= 0 then
        { particle | rotationAcceleration = (force / particle.mass) + particle.rotationAcceleration }

    else
        particle


{-| Derive velocity vector based on old position
-}
velocity : Particle -> Vector2
velocity particle =
    Vector2.subtract particle.oldPosition particle.position


setPosition : Vector2 -> Particle -> Particle
setPosition position particle =
    { particle | position = position, oldPosition = Vector2.subtract (velocity particle) position }


setOrientation : Float -> Particle -> Particle
setOrientation radian particle =
    { particle | orientation = radian }


setMass : Float -> Particle -> Particle
setMass mass particle =
    { particle | mass = max 0 mass }


forwards : Particle -> Vector2
forwards particle =
    Vector2.new (sin particle.orientation) (cos particle.orientation)
        |> Vector2.normalize


{-| Step forwards using Verlet integration
-}
step : Float -> Particle -> Particle
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
