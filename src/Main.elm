module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)



-- MAIN


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    , version : Float
    }


type Page
    = FoldersPage Folders.Model
    | GalleryPage Gallery.Model
    | NotFound


type Route
    = Gallery
    | Folders
    | SelectedPhoto String


init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url { page = NotFound, key = key, version = version }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Gallery ->
            Gallery.init model.version |> toGallery model

        Just Folders ->
            Folders.init Nothing |> toFolders model

        Just (SelectedPhoto filename) ->
            Folders.init (Just filename) |> toFolders model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map SelectedPhoto (s "photos" </> Parser.string)
        , Parser.map Gallery (s "gallery")
        , Parser.map Folders Parser.top
        ]



-- UPDATE


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotFolderMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotFolderMsg folderMessage ->
            case model.page of
                FoldersPage folders ->
                    toFolders model (Folders.update folderMessage folders)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMessage ->
            case model.page of
                GalleryPage gallery ->
                    toGallery model (Gallery.update galleryMessage gallery)

                _ ->
                    ( model, Cmd.none )


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage folders }
    , Cmd.map GotFolderMsg cmd
    )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( gallery, cmd ) =
    ( { model | page = GalleryPage gallery }
    , Cmd.map GotGalleryMsg cmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage gallery ->
            Gallery.subscriptions gallery
                |> Sub.map GotGalleryMsg

        _ ->
            Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage folders ->
                    Folders.view folders
                        |> Html.map GotFolderMsg

                GalleryPage gallery ->
                    Gallery.view gallery
                        |> Html.map GotGalleryMsg

                NotFound ->
                    text "Not found"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader currentPage =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink targetPage { url, caption } =
            li
                [ classList [ ( "active", isActive { link = targetPage, currentPage = currentPage } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Route, currentPage : Page } -> Bool
isActive { link, currentPage } =
    case ( link, currentPage ) of
        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]
