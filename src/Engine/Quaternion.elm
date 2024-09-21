module Engine.Quaternion exposing
    ( Quaternion
    , fromEulerAngles
    , fromVector3
    , identity
    , inverse
    , magnitude
    , multiply
    , normalize
    , xToEuler
    , yToEuler
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


fromVector3 : Vector -> Quaternion
fromVector3 vector =
    Quaternion 0 vector


fromEulerAngles : Float -> Float -> Float -> Quaternion
fromEulerAngles phi theta psi =
    let
        qw : Float
        qw =
            cos (phi * 0.5) * cos (theta * 0.5) * cos (psi * 0.5) + sin (phi * 0.5) * sin (theta * 0.5) * sin (psi * 0.5)

        qx : Float
        qx =
            sin (phi * 0.5) * cos (theta * 0.5) * cos (psi * 0.5) - cos (phi * 0.5) * sin (theta * 0.5) * sin (psi * 0.5)

        qy : Float
        qy =
            cos (phi * 0.5) * sin (theta * 0.5) * cos (psi * 0.5) + sin (phi * 0.5) * cos (theta * 0.5) * sin (psi * 0.5)

        qz : Float
        qz =
            cos (phi * 0.5) * cos (theta * 0.5) * sin (psi * 0.5) - sin (phi * 0.5) * sin (theta * 0.5) * cos (psi * 0.5)
    in
    new qw qx qy qz



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


xToEuler : Quaternion -> Float
xToEuler quaternion =
    let
        t0 : Float
        t0 =
            2 * (quaternion.scalar * quaternion.vector.x + quaternion.vector.y * quaternion.vector.z)

        t1 : Float
        t1 =
            1 - 2 * (quaternion.vector.x * quaternion.vector.x + quaternion.vector.y * quaternion.vector.y)
    in
    atan2 t0 t1


yToEuler : Quaternion -> Float
yToEuler quaternion =
    let
        z : Float
        z =
            zToEuler quaternion

        t2 : Float
        t2 =
            2
                * (quaternion.scalar * quaternion.vector.y - z * quaternion.vector.x)
                |> (\n ->
                        if n > 1 then
                            1

                        else if n < -1 then
                            -1

                        else
                            n
                   )
    in
    asin t2


zToEuler : Quaternion -> Float
zToEuler quaternion =
    let
        t3 : Float
        t3 =
            2 * (quaternion.scalar * quaternion.vector.z + quaternion.vector.x * quaternion.vector.y)

        t4 : Float
        t4 =
            1 - 2 * (quaternion.vector.y * quaternion.vector.y + quaternion.vector.z * quaternion.vector.z)
    in
    atan2 t3 t4
