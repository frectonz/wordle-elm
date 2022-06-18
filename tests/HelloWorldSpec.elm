module HelloWorldSpec exposing (suite)

import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Adder"
        [ Test.test "adds" <|
            \_ ->
                (1 + 1)
                    |> Expect.equal 2
        ]
