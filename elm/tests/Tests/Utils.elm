module Tests.Utils exposing (tests)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (filterBy, filterById, getById, isNothing, replaceBy, replaceAllById)


tests : Test
tests =
    describe "Utils"
        [ isNothingTests
        , replaceByTests
        , replaceAllByIdTests
        , filterByTests
        , filterByIdTests
        , getByIdTests
        ]


isNothingTests : Test
isNothingTests =
    describe "isNothing"
        [ test "Nothing values return true" <|
            \_ ->
                isNothing Nothing
                    |> Expect.true "Expected value to be Nothing."
        , test "Just values return false" <|
            \_ ->
                isNothing (Just 42)
                    |> Expect.false "Expected value to be Just."
        ]


replaceByTests : Test
replaceByTests =
    describe "replaceBy"
        [ test "empty list returns singleton" <|
            \_ -> Expect.equal (replaceBy identity 2 []) [ 2 ]
        , test "list without value returns list with value appended" <|
            \_ -> Expect.equal (replaceBy identity 2 [ 1, 3 ]) [ 1, 3, 2 ]
        , test "list with value returns list with value replaced" <|
            \_ ->
                Expect.equal
                    (replaceBy .id
                        { id = 2, data = "changed" }
                        [ { id = 5, data = "untouched" }, { id = 2, data = "original" } ]
                    )
                    [ { id = 5, data = "untouched" }, { id = 2, data = "changed" } ]
        ]


replaceAllByIdTests : Test
replaceAllByIdTests =
    describe "replaceAllById"
        [ test "empty list returns empty list" <|
            \_ -> Expect.equal (replaceAllById [] [] identity) []
        , test "new data is inserted" <|
            \_ ->
                Expect.equal (replaceAllById [] [ { id = 2, data = "new" } ] identity)
                    [ { id = 2, data = "new" } ]
        , test "existing data is updated" <|
            \_ ->
                Expect.equal
                    (replaceAllById
                        [ { id = 2, data = "original" }
                        , { id = 5, data = "unchanged" }
                        ]
                        [ { id = 2, data = "updated" } ]
                        identity
                    )
                    [ { id = 2, data = "updated" }, { id = 5, data = "unchanged" } ]
        ]


filterByTests : Test
filterByTests =
    describe "filterBy"
        [ test "empty list returns empty list" <|
            \_ -> Expect.equal (filterBy identity 2 []) []
        , test "list without value is empty" <|
            \_ -> Expect.equal (filterBy identity 2 [ 1, 3, 5 ]) []
        , test "list with single value is singleton" <|
            \_ -> Expect.equal (filterBy identity 2 [ 2, 3, 5 ]) [ 2 ]
        , test "list with repeated value contains all matches" <|
            \_ -> Expect.equal (filterBy identity 2 [ 2, 2, 2 ]) [ 2, 2, 2 ]
        ]


filterByIdTests : Test
filterByIdTests =
    describe "filterById"
        [ test "filters by id of value" <|
            \_ ->
                Expect.equal (filterById identity { id = 2 } [ 2, 3, 4 ]) [ 2 ]
        ]


getByIdTests : Test
getByIdTests =
    let
        testId =
            42

        matchingValue =
            { id = testId }
    in
        describe "getById"
            [ test "empty list returns Nothing" <|
                \_ -> Expect.equal (getById [] testId) Nothing
            , test "list containing value returns Just value" <|
                \_ ->
                    Expect.equal
                        (getById [ matchingValue ] testId)
                        (Just matchingValue)
            , test "list without value returns Nothing" <|
                \_ -> Expect.equal (getById [ { id = 4 }, { id = 2 } ] testId) Nothing
            ]
