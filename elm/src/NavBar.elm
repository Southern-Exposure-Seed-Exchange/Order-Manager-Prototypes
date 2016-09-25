module NavBar exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (href, class, id)
import Navigation
import Routing exposing (Route(..))
import Utils exposing (onClickNoDefault)


type Msg
    = Dashboard
    | Products
    | AddProduct
    | Categories
    | AddCategory


update : Msg -> a -> ( a, Cmd b )
update msg model =
    case msg of
        Dashboard ->
            ( model, Navigation.newUrl "#dashboard" )

        Products ->
            ( model, Navigation.newUrl "#products" )

        AddProduct ->
            ( model, Navigation.newUrl "#products/add" )

        Categories ->
            ( model, Navigation.newUrl "#categories" )

        AddCategory ->
            ( model, Navigation.newUrl "#categories/add" )


msgToRoute : Msg -> Route
msgToRoute msg =
    case msg of
        Dashboard ->
            DashboardRoute

        Products ->
            ProductsRoute

        AddProduct ->
            ProductAddRoute

        Categories ->
            CategoriesRoute

        AddCategory ->
            CategoryAddRoute


view : Route -> Html Msg
view currentRoute =
    let
        activeClass msg =
            if isActive msg then
                [ class "active" ]
            else
                []

        isActive msg =
            currentRoute == (msgToRoute msg) || isChildRoute msg

        isChildRoute msg =
            Dict.get (toString msg) childRoutes
                |> Maybe.map (\rs -> List.member currentRoute rs)
                |> Maybe.withDefault False

        childRoutes =
            [ ( Categories, [ AddCategory ] )
            , ( Products, [ AddProduct ] )
            ]
                |> List.map (\( msg, ms ) -> ( toString msg, List.map msgToRoute ms ))
                |> Dict.fromList

        nav =
            node "nav" []

        rootItem msg url name subItems =
            li (activeClass msg)
                [ navLink msg url name
                , ul [ class "submenu" ] subItems
                ]

        navLink msg url name =
            a ([ onClickNoDefault msg, href url ] ++ activeClass msg)
                [ text name ]

        emptyItem name =
            li [] [ a [] [ text name ] ]
    in
        nav
            [ ul
                []
                [ li [ class "text-center", id "site-name" ]
                    [ p [] [ text "OM" ]
                    , small [] [ text "The Enlightened Order Manager" ]
                    ]
                , li [ class "text-center", id "user-links" ]
                    [ p [] [ text "Hello, USERNAME" ]
                    , text "Settings | Logout"
                    ]
                , li []
                    [ navLink Dashboard "#dashboard" "Dashboard" ]
                , rootItem Products
                    "#products"
                    "Products"
                    [ li [] [ navLink AddProduct "#products/add" "Add" ]
                    , emptyItem "Reports"
                    ]
                , rootItem Categories
                    "#categories"
                    "Categories"
                    [ li [] [ navLink AddCategory "#categories/add" "Add" ]
                    , emptyItem "Reports"
                    ]
                , emptyItem "Orders"
                , emptyItem "Customers"
                , emptyItem "Inventory"
                , emptyItem "Stripe"
                , emptyItem "Admin"
                ]
            ]
