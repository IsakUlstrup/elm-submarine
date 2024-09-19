module Engine.Vector exposing
    ( Vector
    , add
    , back
    , cross
    , divide
    , dot
    , getX
    , getY
    , getZ
    , magnitude
    , multiply
    , new
    , normalize
    , pitch
    , right
    , scale
    , setX
    , setY
    , setZ
    , subtract
    , up
    , yaw
    , zero
    )


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }



-- CONSTRUCTORS


new : Float -> Float -> Float -> Vector
new x y z =
    Vector x y z


zero : Vector
zero =
    new 0 0 0



-- DIRECTION UNIT VECTORS


right : Vector
right =
    new 1 0 0


up : Vector
up =
    new 0 1 0


back : Vector
back =
    new 0 0 1



-- OPERATIONS


add : Vector -> Vector -> Vector
add v1 v2 =
    { v1
        | x = v1.x + v2.x
        , y = v1.y + v2.y
        , z = v1.z + v2.z
    }


divide : Float -> Vector -> Vector
divide n vector =
    { vector | x = vector.x / n, y = vector.y / n, z = vector.z / n }


subtract : Vector -> Vector -> Vector
subtract subVector vector =
    { vector
        | x = vector.x - subVector.x
        , y = vector.y - subVector.y
        , z = vector.z - subVector.z
    }


scale : Float -> Vector -> Vector
scale scalar vector =
    { vector
        | x = vector.x * scalar
        , y = vector.y * scalar
        , z = vector.z * scalar
    }


multiply : Vector -> Vector -> Vector
multiply v1 v2 =
    new (v1.x * v2.x) (v1.y * v2.y) (v1.z * v2.z)


dot : Vector -> Vector -> Float
dot u v =
    (getX u * getX v) + (getY u * getY v) + (getZ u * getZ v)


cross : Vector -> Vector -> Vector
cross u v =
    new ((getY u * getZ v) - (getZ u * getY v))
        ((getZ u * getX v) - (getX u * getZ v))
        ((getX u * getY v) - (getY u * getX v))


magnitude : Vector -> Float
magnitude vector =
    dot vector vector |> sqrt


normalize : Vector -> Vector
normalize v =
    scale (1 / magnitude v) v



-- GET/SET


getX : Vector -> Float
getX vector =
    vector.x


setX : Float -> Vector -> Vector
setX component vector =
    { vector | x = component }


getY : Vector -> Float
getY vector =
    vector.y


setY : Float -> Vector -> Vector
setY component vector =
    { vector | y = component }


getZ : Vector -> Float
getZ vector =
    vector.z


setZ : Float -> Vector -> Vector
setZ component vector =
    { vector | z = component }


pitch : Vector -> Float
pitch vector =
    asin vector.y


yaw : Vector -> Float
yaw vector =
    atan2 vector.x vector.z
