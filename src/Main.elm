port module Main exposing (..)

import Browser
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, list, map3, string)
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))



-- HTTP
-- https://quote-garden.herokuapp.com/api/v3/quotes/random
-- https://quote-garden.herokuapp.com/api/v3/quotes
-- https://quote-garden.herokuapp.com/api/v3/quotes?author=bill%20gates
-- https://quote-garden.herokuapp.com/api/v3/quotes?author=AUTHOR


getRandomQuote =
    Http.get
        { url = "https://quote-garden.herokuapp.com/api/v3/quotes/random"
        , expect = Http.expectJson GotRandomQuote quotesListDecoder
        }


getRandomAuthorQuotes : String -> Cmd Msg
getRandomAuthorQuotes author =
    Http.get
        { url = "https://quote-garden.herokuapp.com/api/v3/quotes?author=" ++ author
        , expect = Http.expectJson GotRandomAuthorQuotes quotesListDecoder
        }


type alias QuoteResp =
    { quoteText : String
    , quoteAuthor : String
    , quoteGenre : String
    }


quoteDecoder : Decoder QuoteResp
quoteDecoder =
    map3 QuoteResp
        (field "quoteText" string)
        (field "quoteAuthor" string)
        (field "quoteGenre" string)


quotesListDecoder =
    field "data"
        (list
            quoteDecoder
        )



-- PORTS


port scrollTop : (Int -> msg) -> Sub msg



-- MODEL


type QuotePromise
    = Error
    | Loading
    | Success (List QuoteResp)



-- type Model
--     = SingleQuote QuotePromise
--     | AuthorQuotes QuotePromise


type Views
    = SingleQuote QuotePromise
    | AuthorQuotes QuotePromise


type alias Model =
    { view : Views
    , navbarShadow : Bool
    , scrolledPastAuthor : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { view = SingleQuote Loading, navbarShadow = False, scrolledPastAuthor = False }, getRandomQuote )



-- UPDATE


type Msg
    = GotRandomQuote (Result Http.Error (List QuoteResp))
    | GetAnotherRandomQuote
    | GetRandomAuthorQuotes String
    | GotRandomAuthorQuotes (Result Http.Error (List QuoteResp))
    | UserScrolled Int



-- | GotRandomQuotes (Result Http.Error (List QuotesResp))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomQuote status ->
            case status of
                Ok message ->
                    ( { model | view = SingleQuote (Success message) }, Cmd.none )

                Err _ ->
                    ( { model | view = SingleQuote Error }, Cmd.none )

        GetAnotherRandomQuote ->
            ( { model | view = SingleQuote Loading }, getRandomQuote )

        GetRandomAuthorQuotes author ->
            ( { model | view = AuthorQuotes Loading }, getRandomAuthorQuotes author )

        GotRandomAuthorQuotes status ->
            case status of
                Ok data ->
                    ( { model | view = AuthorQuotes (Success data) }, Cmd.none )

                Err _ ->
                    ( { model | view = AuthorQuotes Error }, Cmd.none )

        UserScrolled amount ->
            ( { model
                | navbarShadow =
                    if amount > 0 then
                        True

                    else
                        False
                , scrolledPastAuthor =
                    if amount > 43 then
                        True

                    else
                        False
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    scrollTop UserScrolled



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ inFront
            (row
                [ width fill
                , height (px 100)
                , Background.color (rgb 255 255 255)
                , padding 20
                , htmlAttribute
                    (class
                        ((if model.navbarShadow then
                            "navbar-shadow"

                          else
                            ""
                         )
                            ++ " navbar"
                        )
                    )
                ]
                [ paragraph
                    [ centerX
                    , paddingEach
                        { top = 0
                        , left = 80
                        , right = 0
                        , bottom = 0
                        }
                    , Font.italic
                    , width
                        (fill
                            |> maximum 800
                        )
                    , Font.size 32
                    , htmlAttribute
                        (class
                            ("navbar-author"
                                ++ (if model.scrolledPastAuthor then
                                        " navbar-author-visible"

                                    else
                                        ""
                                   )
                            )
                        )
                    ]
                    [ text
                        (if model.scrolledPastAuthor then
                            case model.view of
                                SingleQuote status ->
                                    case status of
                                        Success quoteData ->
                                            case List.head quoteData of
                                                Just data ->
                                                    data.quoteAuthor

                                                Nothing ->
                                                    "No quotes."

                                        _ ->
                                            ""

                                AuthorQuotes status ->
                                    case status of
                                        Success quoteData ->
                                            case List.head quoteData of
                                                Just data ->
                                                    data.quoteAuthor

                                                Nothing ->
                                                    "No quotes."

                                        _ ->
                                            ""

                         else
                            ""
                        )
                    ]
                , button [ alignRight, padding 10 ]
                    { onPress = Just GetAnotherRandomQuote
                    , label =
                        row [ spacing 15 ]
                            [ paragraph
                                [ width fill
                                , Font.size 30
                                , Font.family
                                    [ Font.external
                                        { name = "Raleway"
                                        , url = "https://fonts.googleapis.com/css?family=Raleway"
                                        }
                                    , Font.sansSerif
                                    ]
                                ]
                                [ text "random"
                                ]
                            , el [] (Element.html (Filled.autorenew 32 (Color <| Color.rgb255 33 33 33)))
                            ]
                    }
                ]
            )
        ]
        (column [ height fill, width fill ]
            [ case model.view of
                SingleQuote status ->
                    column [ centerX, centerY, padding 50 ]
                        (case status of
                            Success quoteData ->
                                List.map fullQuote quoteData

                            Loading ->
                                [ paragraph [ Font.size 45 ] [ text "Loading..." ] ]

                            Error ->
                                [ text "An error has occurred" ]
                        )

                AuthorQuotes status ->
                    case status of
                        Success quotesData ->
                            column [ centerX, centerY, padding 50, spacing 90 ]
                                (paragraph
                                    [ padding 50
                                    , Font.bold
                                    , Font.size 36
                                    , Font.italic
                                    , Font.color
                                        (fromRgb255
                                            { red = 33
                                            , green = 33
                                            , blue = 33
                                            , alpha = 1
                                            }
                                        )
                                    ]
                                    [ text
                                        (case List.head quotesData of
                                            Just data ->
                                                data.quoteAuthor

                                            Nothing ->
                                                "No quotes."
                                        )
                                    ]
                                    :: List.map quoteCard quotesData
                                )

                        Loading ->
                            column [ centerX, centerY, padding 50 ]
                                [ paragraph [ Font.size 45 ] [ text "Loading..." ] ]

                        Error ->
                            text "An error has occurred"
            ]
        )


fullQuote : QuoteResp -> Element Msg
fullQuote data =
    column [ centerX, centerY, padding 50, spacing 90 ]
        [ quoteCard data
        , quoteFooter data
        ]


quoteCard : QuoteResp -> Element msg
quoteCard data =
    row
        [ centerX
        , width
            (fill
                |> maximum 900
            )
        ]
        [ el [ height fill, width (px 8), Background.color (fromRgb255 { red = 247, green = 223, blue = 148, alpha = 1 }) ]
            (text "")
        , paragraph
            [ width fill
            , Font.size 30
            , paddingEach { top = 0, left = 90, right = 0, bottom = 0 }
            , Font.family
                [ Font.external
                    { name = "Raleway"
                    , url = "https://fonts.googleapis.com/css?family=Raleway"
                    }
                , Font.sansSerif
                ]
            ]
            [ text
                ("“"
                    ++ data.quoteText
                    ++ "”"
                )
            ]
        ]


quoteFooter : QuoteResp -> Element Msg
quoteFooter data =
    button [ width fill, padding 25, htmlAttribute (class "quote-footer-more"), height (px 151) ]
        { onPress = Just (GetRandomAuthorQuotes data.quoteAuthor)
        , label =
            row [ spacing 150, width fill ]
                [ column [ Font.size 24, spacing 5, centerY, width fill ]
                    [ paragraph [ htmlAttribute (class "footer-title") ]
                        [ text data.quoteAuthor ]
                    , paragraph [ htmlAttribute (class "footer-subtitle"), Font.size 14, Font.color (fromRgb255 { red = 130, green = 130, blue = 130, alpha = 1 }) ]
                        [ text data.quoteGenre ]
                    ]
                , el [ htmlAttribute (class "footer-more-arrow") ] (Element.html (Filled.arrow_right_alt 32 (Color <| Color.rgb 91 11 11)))
                ]
        }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
