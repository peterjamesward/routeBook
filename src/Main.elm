module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import FlatColors.BritishPalette
import FlatColors.ChinesePalette exposing (white)
import FlatColors.FlatUIPalette exposing (..)
import Html exposing (Html)
import Markdown
import Random
import Random.List exposing (shuffle)
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type alias Model =
    { active : Maybe Entry
    , entries : List Entry
    }


type alias Flags =
    ()


type Msg
    = SelectEntry (Maybe Entry)
    | Randomized (List Entry)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = Nothing
      , entries = content
      }
    , Random.generate Randomized <| shuffle content
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectEntry entry ->
            ( { model | active = entry }
            , Cmd.none
            )

        Randomized entries ->
            ( { model | entries = entries }
            , Cmd.none
            )


logoImage =
    image [ width <| px 300 ]
        { src = "images/white-roundel_background.png"
        , description = "Gregarios Superclub Ciclista"
        }


view : Model -> Html Msg
view model =
    layout
        [ E.width fill
        , inFront <|
            case model.active of
                Just entry ->
                    el [ centerX, centerY, scrollbarY ] <|
                        entryDetail entry

                Nothing ->
                    none
        ]
    <|
        column [ width fill, spacing 20, padding 20 ]
            [ homeScreen model
            , notes
            ]


notes =
    column [ spacing 10, centerX, padding 20 ] <|
        List.map (paragraph [] << List.singleton)
            [ text "Routes." ]


type Route
    = Home
    | About
    | Evolve
    | NotFound


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.Parser.map Home top
        , Url.Parser.map Home (s "home")
        , Url.Parser.map About (s "about")
        , Url.Parser.map Evolve (s "evolve")
        ]


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse route url)


buyMeACoffeeButton =
    newTabLink
        [ centerX ]
        { url = "https://www.buymeacoffee.com/Peterward"
        , label =
            image [ height (px 60), width (px 217) ]
                { src = "https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png"
                , description = "Buy Me A Coffee"
                }
        }


type alias Entry =
    { title : String
    , content : String
    , gpx : String
    }


content : List Entry
content =
    [ Entry "Windsor"
        """This popular shortish route skims the southern edge of the Chilterns
 before taking to flatter and faster roads including the famour "Museeuw Lane" before
 rolling into Windsor for some signature Cinnamon buns at the reknowned Cinnamon Cafe."""
        "Windsor.gpx"
    ]


homeScreen model =
    wrappedRow
        [ padding 20
        , spacing 20
        , alignLeft
        , alignTop
        ]
    <|
        List.map entryAsHeading model.entries


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


commonStyles =
    [ padding 3
    , E.width <| px 212
    , E.height <| px 212
    , Border.color FlatColors.FlatUIPalette.asbestos
    , Border.width 5
    , Border.rounded 8
    ]


entryAsHeading : Entry -> Element Msg
entryAsHeading entry =
    tricolore 2 <|
        Input.button
            [ spacing 10
            , width (fill |> minimum 300 |> maximum 400)
            , padding 10
            ]
            { onPress = Just <| SelectEntry (Just entry)
            , label =
                paragraph
                    [ Font.size 20
                    , Font.bold
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    ]
                    [ text entry.title ]
            }


withBorder colour width =
    el
        [ Border.color colour
        , Border.width width
        , Border.rounded (width * 4)
        , Background.color white
        ]


tricolore width =
    identity
        << withBorder FlatColors.BritishPalette.downloadProgress width
        << withBorder FlatColors.BritishPalette.lynxWhite width
        << withBorder FlatColors.BritishPalette.nasturcianFlower width


entryDetail : Entry -> Element Msg
entryDetail entry =
    let
        closeButton =
            Input.button
                [ Font.color FlatColors.BritishPalette.chainGangGrey
                , alignRight
                ]
                { onPress = Just <| SelectEntry Nothing
                , label =
                    html <|
                        FeatherIcons.toHtml [] <|
                            FeatherIcons.withSize 36 <|
                                FeatherIcons.x
                }
    in
    tricolore 5 <|
        column
            [ spacing 10, width (fill |> maximum 500), padding 10 ]
            [ row [ width fill ]
                [ paragraph
                    [ Font.size 24
                    , Font.color FlatColors.BritishPalette.electromagnetic
                    , Font.bold
                    , padding 10
                    ]
                    [ text entry.title ]
                , closeButton
                ]
            , paragraph
                [ Font.size 20
                , padding 10
                , Font.color FlatColors.BritishPalette.blueNights
                ]
                [ html <| Markdown.toHtml [] entry.content ]
            ]
