module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes exposing (src)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Element.Border as Border
import Url exposing (..)
import Url.Parser as Parser exposing ( Parser, (</>), int, top, map, oneOf, s, string )
import Url.Parser.Query as Query
import Http
import Json.Decode exposing ( list, string, Decoder, field, maybe, map3, nullable )
import Json.Encode exposing (Value)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import RedHouse.Query as Query
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import RedHouse.Object
import RedHouse.Object.Item as Item
import Graphql.Operation exposing (RootQuery)
import Graphql.Http
import RemoteData exposing (RemoteData)


scaled = modular 16 1.25


---- MODEL ----

type Route
    = ItemsRoute
    | ItemRoute String
    | HomeRoute

routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ItemsRoute top
        , map ItemsRoute (s "items")
        , map ItemRoute (s "item" </> Parser.string )
        , map HomeRoute (s "home")
        ]

type alias Identifiable a =
    { a | id : String }


type alias Spec =
    { specName : String
    , specValue : String
    }

type alias Item =
    { id : String
    , tagLine : String
    , url : Maybe String
    , description : Maybe String
    , specs : Maybe (List String)
    }

type alias User = 
    { id : String
    , userName : String
    , balance : Int
    , profilePicture : Maybe String
    }

type alias Response =
    { focussedItem : Maybe Item
    , items : List Item }

query : String -> SelectionSet Response RootQuery
query itemId =
    SelectionSet.succeed Response 
        |> with (Query.item { id = itemId } itemSelection )
        |> with (Query.items (\optionals -> optionals) itemSelection)

itemSelection : SelectionSet Item RedHouse.Object.Item
itemSelection =
    SelectionSet.succeed Item
        |> with Item.id
        |> with Item.tagLine
        |> with Item.url
        |> with Item.description
        |> with Item.specs

testUser =
    { id = "asdf"
    , userName = "Prasoon"
    , balance = 100
    , profilePicture = Nothing
    }

type UserType = LoggedInUser User
    | AnonymousUser

type alias Model =
    { query : Maybe String
    , apiResult : RemoteData (Graphql.Http.Error Response) Response
    , user : UserType
    , url : Url.Url
    , key : Nav.Key
    }
 

defaultSpecs =
    [ { specName = "Year"
      , specValue = "2019"
      }
    , { specName = "Condition"
      , specValue = "Mint!"
      }
    ]

defaults =
    { itemImage = "http://localhost:3000/logo.svg"
    , profilePicture = "/logo.svg"
    , itemDescription = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    , itemSpecs = defaultSpecs
    }


initialModel : Url.Url -> Nav.Key -> Model
initialModel url key =
    { query = Nothing
    , apiResult = RemoteData.Loading
    , user = LoggedInUser testUser
    , url = url
    , key = key
    }

makeRequest : String -> Cmd Msg
makeRequest itemId =
    query itemId
        |> Graphql.Http.queryRequest "http://localhost:8080/red-house"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( initialModel url key
    , makeRequest ""
    )


---- UPDATE ----


type Msg
    = NoOp 
    | Search String
    | Login
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotResponse (RemoteData (Graphql.Http.Error Response) Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search q ->
            ( { model | query = Just q }, Cmd.none ) 

        Login ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url))
            
                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            case Parser.parse routeParser url of
                Just (ItemRoute id) ->
                    ( { model | url = url }
                    , makeRequest id
                    )
            
                _ ->
                    ( { model | url = url }, Cmd.none )

        GotResponse response ->
            ( { model | apiResult = response }, Cmd.none )
    
        NoOp ->            
            ( model, Cmd.none )


---- VIEW ----

header : Model -> Element Msg
header model =
    row 
    [ width fill 
    , height <| fillPortion 7
    , Border.color <| rgb255 100 10 10
    , Border.solid 
    , Border.glow (rgb255 100 100 100) 1
    , spacing 50
    ]
    [ el [ alignLeft
         , padding 5
         ] 
         (image [ height <| px 50 ] { description = "The red house logo"
                                    , src ="/logo.svg" 
                                    }
         )
    , el [ padding 5
         , Font.heavy
         ]
      (text "RedHouse - Free Market!")
    , row [ alignRight
          , spacing 10
          , padding 20
          ]
          <| userHeaderMenu model.user
    ]

userHeaderMenu : UserType -> List ( Element Msg )
userHeaderMenu user =
    case user of
        AnonymousUser ->
            [ buttonMaker "Register" Nothing
            , buttonMaker "Login" Nothing
            ]
    
        LoggedInUser u ->
            [ column []
                [ row [ padding 5 
                      , spacing 10 
                      ]
                    [ ( image [ height <| px 30
                              , Border.rounded 360
                              ]
                              { description = "User's image"
                              , src = case u.profilePicture of
                                  Just a ->
                                      a
                              
                                  Nothing ->
                                      defaults.profilePicture
                              }
                      )
                    , ( text u.userName )
                    ]
                , el [] (text "Balance: IK 480")
                ]
            ]

buttonMaker : 
    String 
    -> Maybe msg 
    -> Element msg
buttonMaker label msg =
    el [ padding 5
       , spacing 2
       ]
       ( Input.button [ Background.color <| rgb255 64 108 213 
                       , padding 10
                      ]
                      { label = text label
                      , onPress = msg
                      }
         )

searchBar : Model -> Element Msg
searchBar model =
    column
        [ padding 10
        , width <| px 500
        , centerX
        ]
        ( search model.query model )

search : Maybe String -> Model -> List (Element Msg)
search searchQuery model =
    case searchQuery of
        Just q ->
            [ Input.search 
                []
                { onChange = Search
                , text = q
                , placeholder = Just (Input.placeholder [] (text "Search here ..."))
                , label = Input.labelHidden "Search"
                }
                , case model.apiResult of
                    RemoteData.Success a ->
                        searchSuggestions q a.items
                
                    _ ->
                        searchSuggestions q []
            ]
    
        Nothing ->
            [ Input.search 
                []
                { onChange = Search
                , text = ""
                , placeholder = Just (Input.placeholder [] (text "Search here ..."))
                , label = Input.labelHidden "Search"
                }
                , (searchSuggestions "" [])
            ]


searchSuggestions : String -> List Item -> Element Msg
searchSuggestions q items =
    let
        matchedItems = 
            List.map 
            (\n -> n.tagLine) 
            <| filteredItems q items
    in
        column [ Background.color <| rgb255 179 182 231 
               , width <| px 500 
               ]
            <| List.map (\item -> row 
                                      [] 
                                      <| [ link [] 
                                               { url = item
                                               , label = text item
                                                } 
                                          ]
                        ) matchedItems

filteredItems : String 
             -> List { a | tagLine : String } 
             -> List { a | tagLine : String }
filteredItems filterQuery items =
    List.filter 
      (\item -> String.contains filterQuery item.tagLine)
      items

filterOnId : String -> List (Identifiable a) -> List { a | id : String }
filterOnId s list =
    List.filter (\n -> n.id == s) list

content : Model -> Element Msg 
content model =
    case Parser.parse routeParser model.url of
        Just ItemsRoute ->
            row [ width fill
                , height <| fillPortion 10
                ]
                ( case model.apiResult of
                    RemoteData.NotAsked ->
                        []
                
                    RemoteData.Success a ->
                        [ itemContainer a.items ]

                    RemoteData.Loading ->
                        [ ( text "Loading ..." ) ]

                    RemoteData.Failure f ->
                        [ ( text "Failed to fetch data. Please reload." ) ]
                )
    
        Just (ItemRoute id) ->
            wrappedRow 
                [ width fill
                , height fill
                ]
                ( case model.apiResult of
                    RemoteData.NotAsked ->
                        [ (text "Pleae refresh the page!")]
                
                    RemoteData.Success a ->
                        case a.focussedItem of
                            Just i ->
                                [ itemDetail i ]
                        
                            Nothing ->
                                [ (text "Item with this ID not found!")]

                    RemoteData.Loading ->
                        [ (text "Loading ...") ]

                    RemoteData.Failure e ->
                        [ (text "Failed. Please try again.") ]
                )
        _ ->
            row [ width fill
                , height <| fillPortion 10
                ]
                [ (text "404") ]
            
itemDetail : Item -> Element Msg
itemDetail item =
    wrappedRow
        [ width fill ]
        [ column 
            [  width <| fillPortion 3 ]
            [ image
                [ height <| px 250
                , padding 50
                , spacing 50
                ]
                { src = Maybe.withDefault defaults.itemImage item.url
                , description = Maybe.withDefault "Item description not available." item.description
                }
            ]
        , column
            [ width <| fillPortion 5 ]
            [ el 
                [ padding 20
                , Font.size 36
                , Font.family
                    [ Font.typeface "Helvetica"
                    , Font.sansSerif
                    ]
                , Font.italic
                ] 
                (text item.tagLine)
            , el 
                [ padding 30 ] 
                (text (Maybe.withDefault "Item description not available." item.description))
            , paragraph [] [ specsView item.specs ]
            ]
        ]

specsView : Maybe (List String) -> Element Msg
specsView specs =
    textColumn
        []
        (case specs of
            Nothing ->
                []
        
            Just sps ->
                List.map (\spec -> paragraph [] [ text spec ]) sps)

itemTile : Item -> Element Msg
itemTile item =
    column
        [ height <| px 300
        , width <| px 250
        , centerX
        , Background.color <| rgb255 90 90 90
        , Background.image 
            <| case item.url of
                Just a -> a
                Nothing -> defaults.itemImage
        ]
        [ paragraph 
            [ Background.color <| rgba255 53 66 75 0.75 ] 
            [ text item.tagLine ]
        , el [ alignBottom
             , width fill
             ]
             ( link [ width fill ]
                 { url = "/item/" ++ item.id
                 , label = text "Look Inside"
                 }
             )
            --  (Input.button 
            --         [ Background.color <| rgba255 53 66 75 0.75
            --         , width fill
            --         , height <| px 50
            --         ]
            --         { label = text "Look inside"
            --         , onPress = case fromString "/item/$item.id" of
            --             Just a ->
            --                 UrlChanged a
                    
            --             Nothing ->
            --                 NoOp
            --         }
            --     ) 
        ]

itemContainer : List Item -> Element Msg
itemContainer itemsL =
    wrappedRow 
    [ width fill
    , padding 30
    , spacing 30
    , scrollbarY
    ] 
    <| List.map itemTile itemsL

footer : Element Msg
footer =
    row
    [ width fill 
    , height <| fillPortion 2
    , spacing 50
    , padding 50
    ]
    [ el  [ centerX
          --, Font.size (scaled 1)
          ] (text "Made with love using Elm and Scala") ]



view : Model -> Browser.Document Msg
view model =
    { title = "RedHouse - Share if you care!"
    , body = body model
    }

body : Model -> List (Html Msg)
body model =
    [ layout [ Font.family  
                [ Font.typeface "Gotham Rounded A"
                , Font.sansSerif
                ]
            ] <|
     column [ height fill
            , width fill
            , spacing 20
            ]
         [ header model
         , searchBar model
         , content model
         , footer
         ]
    ]


---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

---- PROGRAM ----
main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }    