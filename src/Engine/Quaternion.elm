module Engine.Quaternion exposing
    ( Quaternion
    , identity
    , inverse
    , magnitude
    , multiply
    , new
    , normalize
    , xRotation
    , xToEuler
    , yRotation
    , yToEuler
    , zRotation
    , zToEuler
    )

import Engine.Vector as Vector exposing (Vector)


type alias Quaternion =
    { scalar : Float
    , vector : Vector
    }



-- CONSTRUCTORS


identity : Quaternion
identity =
    Quaternion 1 Vector.zero


new : Float -> Float -> Float -> Float -> Quaternion
new scalar x y z =
    Quaternion scalar (Vector.new x y z)


xRotation : Float -> Quaternion
xRotation angle =
    new (cos (0.5 * angle)) (sin (0.5 * angle)) 0 0


{-| Create a quaternion corresponding to a rotation about the y axis by the given angle.
-}
yRotation : Float -> Quaternion
yRotation angle =
    new (cos (0.5 * angle)) 0 (sin (0.5 * angle)) 0


{-| Create a quaternion corresponding to a rotation about the z axis by the given angle.
-}
zRotation : Float -> Quaternion
zRotation angle =
    new (cos (0.5 * angle)) 0 0 (sin (0.5 * angle))


fromEuler : Float -> Float -> Float -> Quaternion
fromEuler x y z =
    let
        cr =
            cos (z * 0.5)

        sr =
            sin (z * 0.5)

        cp =
            cos (x * 0.5)

        sp =
            sin (x * 0.5)

        cy =
            cos (y * 0.5)

        sy =
            sin (y * 0.5)
    in
    new
        (cr * cp * cy + sr * sp * sy)
        (sr * cp * cy - cr * sp * sy)
        (cr * sp * cy + sr * cp * sy)
        (cr * cp * sy - sr * sp * cy)



-- OPERATIONS


magnitude : Quaternion -> Float
magnitude quaternion =
    Vector.magnitude quaternion.vector
        + (quaternion.scalar ^ 2)
        |> sqrt


normalize : Quaternion -> Quaternion
normalize q =
    -- if magnitude q == 0 then
    --     Nothing
    -- else
    --     Just
    scale (1 / magnitude q) q


scale : Float -> Quaternion -> Quaternion
scale n quaternion =
    { quaternion
        | scalar = quaternion.scalar * n
        , vector = Vector.scale n quaternion.vector
    }


inverse : Quaternion -> Quaternion
inverse quaternion =
    { quaternion | vector = Vector.scale -1 quaternion.vector }


multiply : Quaternion -> Quaternion -> Quaternion
multiply p q =
    { scalar = q.scalar * p.scalar - Vector.dot q.vector p.vector
    , vector =
        Vector.scale q.scalar p.vector
            |> Vector.add (Vector.scale p.scalar q.vector)
            |> Vector.add (Vector.cross p.vector q.vector)
    }



-- TO EULER


yToEuler : Quaternion -> Float
yToEuler quaternion =
    let
        sinp =
            sqrt (1 + 2 * (quaternion.scalar * quaternion.vector.y - quaternion.vector.x * quaternion.vector.z))

        cosp =
            sqrt (1 - 2 * (quaternion.scalar * quaternion.vector.y - quaternion.vector.x * quaternion.vector.z))
    in
    2 * atan2 sinp cosp - pi / 2


zToEuler : Quaternion -> Float
zToEuler quaternion =
    let
        siny_cosp =
            2 * (quaternion.scalar * quaternion.vector.z + quaternion.vector.x * quaternion.vector.y)

        cosy_cosp =
            1 - 2 * (quaternion.vector.y * quaternion.vector.y + quaternion.vector.z * quaternion.vector.z)
    in
    atan2 siny_cosp cosy_cosp


xToEuler : Quaternion -> Float
xToEuler quaternion =
    let
        sinr_cosp =
            2 * (quaternion.scalar * quaternion.vector.x + quaternion.vector.y * quaternion.vector.z)

        cosr_cosp =
            1 - 2 * (quaternion.vector.x * quaternion.vector.x + quaternion.vector.y * quaternion.vector.y)
    in
    atan2 sinr_cosp cosr_cosp
