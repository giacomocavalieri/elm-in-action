module PhotoFolders exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, h3, img, label, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
        }


type FolderPath
    = End
    | Subfolder Int FolderPath


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root =
        Folder
            { name = "Loading..."
            , photoUrls = []
            , subfolders = []
            , expanded = True
            }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        , root =
            Folder
                { name = "Photos"
                , photoUrls = []
                , subfolders =
                    [ Folder
                        { name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                , expanded = True
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = [ "fresco" ]
                                , subfolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    , Folder
                        { name = "2017"
                        , photoUrls = []
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                , expanded = True
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = []
                                , subfolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    ]
                , expanded = True
                }
        }



-- UPDATE


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )

        ClickedFolder folderPath ->
            ( { model | root = toggleExpanded folderPath model.root }, Cmd.none )


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }



-- VIEW


view : Model -> Html Msg
view model =
    let
        selectedPhoto =
            model.selectedPhotoUrl
                |> Maybe.andThen (\url -> Dict.get url model.photos)
                |> Maybe.map viewSelectedPhoto
                |> Maybe.withDefault (text "")
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (ClickedPhoto url) ]
        [ text url ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (toSelectedUrl photo.url) ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (toThumbnailUrl url)
        ]
        []


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


toSelectedUrl : String -> String
toSelectedUrl url =
    urlPrefix ++ "photos/" ++ url ++ "/full"


toThumbnailUrl : String -> String
toThumbnailUrl url =
    urlPrefix ++ "photos/" ++ url ++ "/thumb"


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]

        contents =
            List.append
                (List.indexedMap viewSubfolder folder.subfolders)
                (List.map viewPhoto folder.photoUrls)
    in
    if folder.expanded then
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)
