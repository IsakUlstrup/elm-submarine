module Engine.Vector2 exposing
    ( Vector2
    , add
    , angleDegrees
    , angleRadian
    , direction
    , distance
    , divide
    , east
    , magnitude
    , mapX
    , mapY
    , new
    , normalize
    , orthogonal
    , rotate
    , scale
    , subtract
    , zero
    )


type alias Vector2 =
    { x : Float
    , y : Float
    }


new : Float -> Float -> Vector2
new x y =
    Vector2 x y


{-| Map x component
-}
mapX : (Float -> Float) -> Vector2 -> Vector2
mapX f vector =
    { vector | x = f vector.x }


{-| Map y component
-}
mapY : (Float -> Float) -> Vector2 -> Vector2
mapY f vector =
    { vector | y = f vector.y }


{-| Add the components of two vectors together
-}
add : Vector2 -> Vector2 -> Vector2
add v1 v2 =
    { v1 | x = v1.x + v2.x, y = v1.y + v2.y }


{-| Subtract the components of sub from vector
-}
subtract : Vector2 -> Vector2 -> Vector2
subtract sub vector =
    { vector | x = vector.x - sub.x, y = vector.y - sub.y }


{-| Multiply components by given value
-}
scale : Float -> Vector2 -> Vector2
scale amount vector =
    { vector | x = vector.x * amount, y = vector.y * amount }


{-| Divide components by given value
-}
divide : Float -> Vector2 -> Vector2
divide value vector =
    if value == 0 then
        zero

    else
        { vector | x = vector.x / value, y = vector.y / value }


magnitude : Vector2 -> Float
magnitude vector =
    sqrt (vector.x ^ 2 + vector.y ^ 2)


{-| Normalize a vector, a normalized vector is one where length/manitude is 1
-}
normalize : Vector2 -> Vector2
normalize vector =
    divide (magnitude vector) vector


{-| Get distance between two vectors
-}
distance : Vector2 -> Vector2 -> Float
distance v1 v2 =
    sqrt (((v1.x - v2.x) ^ 2) + ((v1.y - v2.y) ^ 2))


{-| Returns a normalized vector pointing from origin to target
-}
direction : Vector2 -> Vector2 -> Vector2
direction origin target =
    Vector2 (target.x - origin.x) (target.y - origin.y) |> normalize


{-| Construct new vector with both components set to 0
-}
zero : Vector2
zero =
    new 0 0


rotate : Float -> Vector2 -> Vector2
rotate theta vector =
    { vector
        | x = (vector.x * cos theta) - (vector.y * sin theta)
        , y = (vector.x * sin theta) + (vector.y * cos theta)
    }


angleRadian : Vector2 -> Float
angleRadian { x, y } =
    atan2 x y


angleDegrees : Vector2 -> Float
angleDegrees vector =
    (angleRadian vector * 180) / pi


east : Vector2
east =
    Vector2 1 0


orthogonal : Vector2 -> Vector2
orthogonal { x, y } =
    new y -x
