module Engine.Orientation exposing (..)


type Radian
    = Radian Float


newAngle : Float -> Radian
newAngle angle =
    let
        clamp x =
            if x > 360 then
                clamp (x - 360)

            else if x < 0 then
                clamp (360 - x)

            else
                x
    in
    Radian (angle |> clamp |> degrees)


toDegrees : Radian -> Float
toDegrees (Radian radian) =
    radian * 180 / pi


type alias Orientation =
    { pitch : Radian
    , yaw : Radian
    , roll : Radian
    }


new : Orientation
new =
    Orientation (newAngle 0) (newAngle 0) (newAngle 0)
