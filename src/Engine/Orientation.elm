module Engine.Orientation exposing
    ( Orientation
    , new
    , pitch
    , pitchAngle
    , roll
    , rollAngle
    , toVector
    , yaw
    , yawAngle
    )

import Engine.Vector as Vector exposing (Vector)


type alias Orientation =
    { pitch : Radian
    , yaw : Radian
    , roll : Radian
    }


new : Orientation
new =
    Orientation (newAngle 0) (newAngle 0) (newAngle 0)


pitch : Float -> Orientation -> Orientation
pitch angle orientation =
    { orientation | pitch = addAngle (newAngle angle) orientation.pitch }


yaw : Float -> Orientation -> Orientation
yaw angle orientation =
    { orientation | yaw = addAngle (newAngle angle) orientation.yaw }


roll : Float -> Orientation -> Orientation
roll angle orientation =
    { orientation | roll = addAngle (newAngle angle) orientation.roll }


pitchAngle : Orientation -> Float
pitchAngle orientation =
    toDegrees orientation.pitch


yawAngle : Orientation -> Float
yawAngle orientation =
    toDegrees orientation.yaw


rollAngle : Orientation -> Float
rollAngle orientation =
    toDegrees orientation.roll


toVector : Orientation -> Vector
toVector orientation =
    let
        p =
            toFloat orientation.pitch

        y =
            toFloat orientation.yaw
    in
    Vector.new (sin y) -(sin p * cos y) -(cos p * cos y)



-- RADIAN


type Radian
    = Radian Float


{-| New radian from angle in degrees
-}
newAngle : Float -> Radian
newAngle angle =
    let
        clamp x =
            if x > 360 then
                clamp (x - 360)

            else if x < 0 then
                clamp (360 + x)

            else
                x
    in
    Radian (clamp angle / 180)


addAngle : Radian -> Radian -> Radian
addAngle (Radian r1) (Radian r2) =
    let
        clamp x =
            if x > 2 then
                clamp (x - 2)

            else if x < 0 then
                clamp (2 - x)

            else
                x
    in
    Radian (r1 + r2 |> clamp)



-- from degrees: a / 180
-- to degrees: r * 180
-- to rad: r * pi


toFloat : Radian -> Float
toFloat (Radian rad) =
    rad


toDegrees : Radian -> Float
toDegrees (Radian radian) =
    radian * 180
