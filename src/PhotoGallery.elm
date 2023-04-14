port module PhotoGallery exposing (Model, Msg(..), Photo, Status(..), init, photoDecoder, subscriptions, update, urlPrefix, view)

import Html exposing (Attribute, Html, button, canvas, div, h3, img, input, label, node, text)
import Html.Attributes as Attr exposing (checked, class, classList, id, name, property, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random



-- MODEL


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


maxValue : number
maxValue =
    11


type Status
    = Loading
    | Loaded (List Photo) Photo
    | Error String


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        initialModel =
            { status = Loading
            , activity = "Initializing Pasta v" ++ String.fromFloat flags
            , chosenSize = Medium
            , hue = 5
            , ripple = 5
            , noise = 5
            }

        initialCommand =
            Http.get
                { url = "http://elm-in-action.com/photos/list.json"
                , expect = Http.expectJson GotPhotos (list photoDecoder)
                }
    in
    ( initialModel, initialCommand )



-- PORTS


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity



-- UPDATE


type Msg
    = SelectedPhoto Photo
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotPhotos (Result Http.Error (List Photo))
    | GotActivity String
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SelectedPhoto photo ->
            applyFilters { model | status = selectPhoto photo model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    let
                        generator =
                            Random.uniform firstPhoto otherPhotos

                        cmd =
                            Random.generate SelectedPhoto generator
                    in
                    ( model, cmd )

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotPhotos (Ok ((firstPhoto :: _) as photos)) ->
            applyFilters { model | status = Loaded photos firstPhoto }

        GotPhotos (Ok []) ->
            ( { model | status = Error "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Error "Server error!" }, Cmd.none )

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ photo ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / maxValue }
                    , { name = "Ripple", amount = toFloat model.ripple / maxValue }
                    , { name = "Noise", amount = toFloat model.noise / maxValue }
                    ]

                url =
                    urlPrefix ++ "large/" ++ photo.url

                cmd =
                    setFilters { url = url, filters = filters }
            in
            ( model, cmd )

        Loading ->
            ( model, Cmd.none )

        Error _ ->
            ( model, Cmd.none )


selectPhoto : Photo -> Status -> Status
selectPhoto photo status =
    case status of
        Loaded photos _ ->
            Loaded photos photo

        Loading ->
            status

        Error _ ->
            status



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loading ->
                [ text "Loading..." ]

            Loaded photos selectedPhoto ->
                viewLoaded photos selectedPhoto model

            Error errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


sizeToString : ThumbnailSize -> String
sizeToString thumbnailSize =
    case thumbnailSize of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewLoaded : List Photo -> Photo -> Model -> List (Html Msg)
viewLoaded photos selectedPhoto model =
    [ button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , div [ class "activity" ] [ text model.activity ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedPhoto) photos)
    , canvas [ id "main-canvas", class "large" ] []
    ]


viewThumbnail : Photo -> Photo -> Html Msg
viewThumbnail selectedPhoto thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ String.fromInt thumbnail.size ++ " KB]")
        , classList [ ( "selected", selectedPhoto == thumbnail ) ]
        , onClick (SelectedPhoto thumbnail)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize thumbnailSize =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize thumbnailSize)
            , checked (thumbnailSize == selectedSize)
            ]
            []
        , text (sizeToString thumbnailSize)
        ]


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max (String.fromInt maxValue)
            , property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Decode.map toMsg
        |> on "slide"
