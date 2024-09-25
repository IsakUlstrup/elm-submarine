module OrientationTest exposing (..)

import Engine.Orientation as Orientation
import Expect
import Test exposing (Test, describe, test)


rotation : Test
rotation =
    describe "Rotations"
        [ test "Pitch 90 degrees" <|
            \_ ->
                Orientation.new
                    |> Orientation.pitch 90
                    |> Orientation.pitchAngle
                    |> Expect.equal 90
        , test "Pitch -90 degrees" <|
            \_ ->
                Orientation.new
                    |> Orientation.pitch -90
                    |> Orientation.pitchAngle
                    |> Expect.equal 270
        , test "Pitch 540 degrees, should result in 180 degrees" <|
            \_ ->
                Orientation.new
                    |> Orientation.pitch 540
                    |> Orientation.pitchAngle
                    |> Expect.equal 180
        ]
