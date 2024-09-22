module Rotation exposing (..)

import Engine.Quaternion as Quaternion exposing (Quaternion)
import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)


quaternionIsSimilar : Quaternion -> Quaternion -> Expect.Expectation
quaternionIsSimilar target quaternion =
    Expect.all
        [ \q -> Expect.within (Absolute 0.001) target.scalar q.scalar
        , \q -> Expect.within (Absolute 0.001) target.vector.x q.vector.x
        , \q -> Expect.within (Absolute 0.001) target.vector.y q.vector.y
        , \q -> Expect.within (Absolute 0.001) target.vector.z q.vector.z
        ]
        quaternion


constructor : Test
constructor =
    describe "Constructors"
        -- Nest as many descriptions as you like.
        [ test "Construct quaternion with 10degrees rotation on x axis" <|
            \_ ->
                Quaternion.xRotation (degrees 10)
                    |> quaternionIsSimilar (Quaternion.new 0.996 0.087 0 0)
        , test "Construct quaternion with 10degrees rotation on y axis" <|
            \_ ->
                Quaternion.yRotation (degrees 10)
                    |> quaternionIsSimilar (Quaternion.new 0.996 0 0.087 0)
        , test "Construct quaternion with 10degrees rotation on z axis" <|
            \_ ->
                Quaternion.zRotation (degrees 10)
                    |> quaternionIsSimilar (Quaternion.new 0.996 0 0 0.087)

        -- Construct from euler, convert back
        , test "Construct quaternion with 10degrees rotation on x axis, convert back to angle. Should be 10" <|
            \_ ->
                Quaternion.xRotation (degrees 10)
                    |> Quaternion.xToEuler
                    |> (\rad -> (rad * 180) / pi)
                    |> Expect.within (Absolute 0.001) 10
        , test "Construct quaternion with 10degrees rotation on y axis, convert back to angle. Should be 10" <|
            \_ ->
                Quaternion.yRotation (degrees 10)
                    |> Quaternion.yToEuler
                    |> (\rad -> (rad * 180) / pi)
                    |> Expect.within (Absolute 0.001) 10
        , test "Construct quaternion with 10degrees rotation on z axis, convert back to angle. Should be 10" <|
            \_ ->
                Quaternion.zRotation (degrees 10)
                    |> Quaternion.zToEuler
                    |> (\rad -> (rad * 180) / pi)
                    |> Expect.within (Absolute 0.001) 10
        ]



-- rotation : Test
-- rotation =
--     describe "Rotation tests"
--         [ test "Rotate identity quaternion 10 degrees on x axis" <|
--             \_ ->
--                 Quaternion.fromEuler (degrees 10) 0 0
--                     |> Quaternion.multiply Quaternion.identity
--                     |> Quaternion.xToEuler
--                     |> (\rad -> (rad * 180) / pi)
--                     |> Expect.within (Absolute 0.001) 10
--         , test "Rotate identity quaternion 45deg twice on x axis" <|
--             \_ ->
--                 Quaternion.fromEuler (degrees 45) 0 0
--                     |> Quaternion.multiply Quaternion.identity
--                     |> Quaternion.multiply (Quaternion.fromEuler (degrees 45) 0 0)
--                     |> Quaternion.xToEuler
--                     |> (\rad -> (rad * 180) / pi)
--                     |> Expect.within (Absolute 0.001) 90
--         , test "Rotate identity quaternion 45deg and back -45deg on y axis" <|
--             \_ ->
--                 Quaternion.fromEuler 0 (degrees 45) 0
--                     |> Quaternion.multiply Quaternion.identity
--                     |> Quaternion.multiply (Quaternion.fromEuler 0 (degrees -45) 0)
--                     |> Quaternion.yToEuler
--                     |> (\rad -> (rad * 180) / pi)
--                     |> Expect.within (Absolute 0.001) 0
--         , test "Rotate identity quaternion 90deg four times on y axis" <|
--             \_ ->
--                 Quaternion.fromEuler 0 (degrees 90) 0
--                     |> Quaternion.multiply Quaternion.identity
--                     |> Quaternion.multiply (Quaternion.fromEuler 0 (degrees 90) 0)
--                     |> Quaternion.multiply (Quaternion.fromEuler 0 (degrees 90) 0)
--                     |> Quaternion.multiply (Quaternion.fromEuler 0 (degrees 90) 0)
--                     |> Quaternion.yToEuler
--                     |> (\rad -> (rad * 180) / pi)
--                     |> Expect.within (Absolute 0.001) 0
--         , test "Rotate both x and y 10deg" <|
--             \_ ->
--                 Quaternion.fromEuler (degrees 10) (degrees 10) 0
--                     |> Quaternion.multiply Quaternion.identity
--                     |> Quaternion.xToEuler
--                     |> (\rad -> (rad * 180) / pi)
--                     |> Expect.within (Absolute 0.001) 10
--         ]
