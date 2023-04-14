module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, string)
import Html.Attributes as Attr
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGallery exposing (Model, Msg(..), Photo, Status(..), urlPrefix)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag)


photoDecoderTest : Test
photoDecoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGallery.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


initialModel : Model
initialModel =
    Tuple.first (PhotoGallery.init 0)


testSliders : Test
testSliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> PhotoGallery.update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render" <|
        \_ ->
            initialModel
                |> PhotoGallery.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


withRandomPhotos : String -> (List Photo -> Photo -> List Photo -> Query.Single Msg -> Expectation) -> Test
withRandomPhotos description check =
    fuzz photosFuzzer description <|
        \( before, selected, after ) ->
            let
                allPhotos =
                    before ++ selected :: after
            in
            { initialModel | status = Loaded allPhotos selected }
                |> PhotoGallery.view
                |> Query.fromHtml
                |> check before selected after


photosFuzzer : Fuzzer ( List Photo, Photo, List Photo )
photosFuzzer =
    let
        beforeFuzzer =
            Fuzz.map (photosFromCount "before") (Fuzz.intRange 1 5)

        afterFuzzer =
            Fuzz.map (photosFromCount "after") (Fuzz.intRange 1 5)

        selected =
            photoFromUrl "selected.png"
    in
    Fuzz.map2 (\before after -> ( before, selected, after )) beforeFuzzer afterFuzzer


photosFromCount : String -> Int -> List Photo
photosFromCount prefix photosCount =
    List.range 1 photosCount
        |> List.map String.fromInt
        |> List.map (\num -> prefix ++ num ++ ".png")
        |> List.map photoFromUrl


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    withRandomPhotos "URLs render as thumbnails" <|
        \before selected after ->
            (before ++ selected :: after)
                |> List.map .url
                |> List.map thumbnailRendered
                |> Expect.all


firstPhotoIsSelected : Test
firstPhotoIsSelected =
    withRandomPhotos "First photo is selected" <|
        \_ selected _ query ->
            query
                |> Query.find [ tag "img", attribute (Attr.src (urlPrefix ++ selected.url)) ]
                |> Query.has [ class "selected" ]


clickThumbnail : Test
clickThumbnail =
    withRandomPhotos "Clicking a thumbnail selects it" <|
        \_ selected _ query ->
            let
                srcToClick =
                    urlPrefix ++ selected.url
            in
            query
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (SelectedPhoto selected)
