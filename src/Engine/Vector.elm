module Engine.Vector exposing
    ( Vector
    , add
    , back
    , cross
    , divide
    , dot
    , magnitude
    , multiply
    , new
    , normalize
    , right
    , scale
    , subtract
    , up
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
    (u.x * v.x) + (u.y * v.y) + (u.z * v.z)


cross : Vector -> Vector -> Vector
cross u v =
    new ((u.y * v.z) - (u.z * v.y))
        ((u.z * v.x) - (u.x * v.z))
        ((u.x * v.y) - (u.y * v.x))


magnitude : Vector -> Float
magnitude vector =
    dot vector vector |> sqrt


normalize : Vector -> Vector
normalize v =
    scale (1 / magnitude v) v
