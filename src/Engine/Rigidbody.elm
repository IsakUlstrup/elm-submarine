module Engine.Rigidbody exposing
    ( Rigidbody
    , applyForce
    , new
    , rotate
    , setMass
    , setOrientation
    , setPosition
    , step
    , translate
    , translateRelative
    , velocity
    )

import Engine.Quaternion as Quaternion exposing (Quaternion)
import Engine.Vector as Vector exposing (Vector)


{-| A particle meant to be used with Verlet integration
-}
type alias Rigidbody =
    { position : Vector
    , oldPosition : Vector
    , acceleration : Vector
    , orientation : Quaternion
    , mass : Float
    }


{-| Particle constructor
-}
new : Vector -> Float -> Rigidbody
new position mass =
    Rigidbody position position Vector.zero Quaternion.identity mass


applyForce : Vector -> Rigidbody -> Rigidbody
applyForce force body =
    if body.mass /= 0 then
        { body
            | acceleration =
                force
                    |> Vector.divide body.mass
                    |> Vector.add body.acceleration
        }

    else
        body


{-| Derive velocity vector based on old position
-}
velocity : Rigidbody -> Vector
velocity body =
    Vector.subtract body.oldPosition body.position


setMass : Float -> Rigidbody -> Rigidbody
setMass mass body =
    { body | mass = max 0 mass }


{-| Step forwards using Verlet integration
-}
step : Float -> Rigidbody -> Rigidbody
step dt body =
    { body
        | position =
            body.position
                |> Vector.add (velocity body)
                |> Vector.add (Vector.scale (dt ^ 2) body.acceleration)
        , oldPosition = body.position
        , acceleration = Vector.zero
    }


setPosition : Vector -> Rigidbody -> Rigidbody
setPosition position body =
    { body
        | position = position
        , oldPosition = Vector.subtract (velocity body) position
    }


translate : Vector -> Rigidbody -> Rigidbody
translate vector body =
    { body
        | position = Vector.add body.position vector
    }


rotate : Quaternion -> Rigidbody -> Rigidbody
rotate quaternion body =
    { body | orientation = Quaternion.multiply quaternion body.orientation }


translateRelative : Vector -> Rigidbody -> Rigidbody
translateRelative vector body =
    let
        rotatedVector : Vector
        rotatedVector =
            vectorMultiply body.orientation vector
    in
    { body
        | position = Vector.add body.position rotatedVector
        , oldPosition = Vector.add body.position rotatedVector
    }


setOrientation : Quaternion -> Rigidbody -> Rigidbody
setOrientation matrix body =
    { body | orientation = matrix }


vectorMultiply : Quaternion -> Vector -> Vector
vectorMultiply quaternion vector =
    Quaternion.multiply (Quaternion.multiply quaternion (Quaternion 0 vector)) (Quaternion.inverse quaternion)
        |> .vector
