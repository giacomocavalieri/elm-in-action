module PhotoFolders exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, h3, img, span, text)
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
    }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
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
        }



-- UPDATE


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )



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
        [ div [ class "selected-photo" ] [ selectedPhoto ] ]


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
