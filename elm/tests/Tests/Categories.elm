module Tests.Categories exposing (tests)

import Expect
import Test exposing (Test, describe, test)
import Api.Models exposing (initialCategory, initialProduct)
import Categories.List exposing (childCount, productCount)


tests : Test
tests =
    describe "Categories"
        [ childCountTests
        , productCountTests
        ]


childCountTests : Test
childCountTests =
    let
        testCategory =
            { initialCategory | id = 42 }

        childCategory =
            { initialCategory | id = 9001, parent = Just testCategory.id }
    in
        describe "childCount"
            [ test "empty list returns 0" <|
                \_ -> Expect.equal (childCount [] testCategory) 0
            , test "list without child returns 0" <|
                \_ -> Expect.equal (childCount [ testCategory ] testCategory) 0
            , test "list with child of other parent returns 0" <|
                \_ ->
                    Expect.equal
                        (childCount
                            [ testCategory
                            , { childCategory | parent = Just 99 }
                            ]
                            testCategory
                        )
                        0
            , test "list with single child returns 1" <|
                \_ ->
                    Expect.equal
                        (childCount
                            [ testCategory
                            , childCategory
                            ]
                            testCategory
                        )
                        1
            , test "list with two children returns 2" <|
                \_ ->
                    Expect.equal
                        (childCount
                            [ testCategory
                            , childCategory
                            , { childCategory | id = 9002 }
                            ]
                            testCategory
                        )
                        2
            ]


productCountTests : Test
productCountTests =
    let
        testCategory =
            { initialCategory | id = 42 }

        childProduct =
            { initialProduct | id = 99, category = testCategory.id }
    in
        describe "productCount"
            [ test "empty list returns 0" <|
                \_ -> Expect.equal (productCount [] testCategory) 0
            , test "list without child product returns 0" <|
                \_ ->
                    Expect.equal
                        (productCount [ { childProduct | category = 9001 } ]
                            testCategory
                        )
                        0
            , test "list with one child product returns 1" <|
                \_ -> Expect.equal (productCount [ childProduct ] testCategory) 1
            , test "list with two children product returns 2" <|
                \_ ->
                    Expect.equal
                        (productCount [ childProduct, { childProduct | id = 100 } ]
                            testCategory
                        )
                        2
            ]
