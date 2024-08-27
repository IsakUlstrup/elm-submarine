module Engine.Particle exposing
    ( Particle
    , applyForce
    , applyRotationalForce
    , new
    , setMass
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
    , orientation : Vector2
    , rotationVelocity : Float
    , rotationAcceleration : Float
    , mass : Float
    , radius : Float
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Particle
new position mass =
    Particle position position Vector2.zero Vector2.east 0 0 mass 15


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
        { particle | rotationAcceleration = particle.rotationAcceleration + (force / particle.mass) }

    else
        particle



-- applyGravity : Vector2 -> Particle -> Particle
-- applyGravity force particle =
--     if particle.mass /= 0 then
--         { particle | acceleration = Vector2.add particle.acceleration force }
--     else
--         particle


{-| Derive velocity vector based on old position
-}
velocity : Particle -> Vector2
velocity particle =
    Vector2.subtract particle.oldPosition particle.position



-- forwards : Particle -> Vector2
-- forwards particle =
--     Vector2.new (cos particle.rotation) (sin particle.rotation)
--         |> Vector2.add particle.position
--         |> Vector2.direction particle.position


setPosition : Vector2 -> Particle -> Particle
setPosition position particle =
    { particle | position = position, oldPosition = Vector2.subtract (velocity particle) position }


setMass : Float -> Particle -> Particle
setMass mass particle =
    { particle | mass = max 0 mass }



-- clampRadian : Float -> Float
-- clampRadian radian =
--     if radian > pi * 2 then
--         0 + (radian - pi * 2)
--     else if radian < 0 then
--         (pi * 2) - radian
--     else
--         radian


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
        , orientation = Vector2.rotate particle.rotationVelocity particle.orientation
        , rotationAcceleration = 0
    }



-- relativeVelocity : Particle a -> Particle a -> Vector2
-- relativeVelocity target particle =
--     Vector2.subtract (velocity target) (velocity particle)
