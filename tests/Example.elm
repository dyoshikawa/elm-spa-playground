module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Route exposing (Route, parse)
import Test exposing (..)
import Url


suite : Test
suite =
    describe "route"
        [ test "Should parse URL" <|
            \_ ->
                Url.fromString "http://example.com/"
                    |> Maybe.andThen Route.parse
                    |> Expect.equal (Just Route.Top)
        ]
