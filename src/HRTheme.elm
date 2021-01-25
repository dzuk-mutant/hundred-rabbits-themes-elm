module HRTheme exposing (HRTheme, decoder, toXmlString, toSvgImage)

{-| Decode theme files and use themes that conform to the
[Hundred Rabbits theme framework](https://github.com/hundredrabbits/Themes).

@docs HRTheme, decoder, toXmlString, toSvgImage
-}

import Color exposing (Color)
import Color.Convert exposing (hexToColor, colorToHex)
import Dict
import Svg exposing (Svg, svg, rect, circle)
import Svg.Attributes exposing (xmlBase, id, fill, width, height, cx, cy, r, version, baseProfile)
import Xml.Decode as XD exposing (fail, list, map2, path, stringAttr, succeed)



{-| A color theme that uses the Hundred Rabbits theme spec.

Because there's no central Color type in elm, you may have
to convert these colours to another type when you get them
to be usable.

The Color types used in this record are from `avh4/elm-color`.

Because it would be impractical to do so,
there is no constructor function for this type, you just manually
create the record like so:

    import Color exposing (rgb255) -- avh4/elm-color

    myTheme : HRTheme
    myTheme = { background = rgb255 224 177 203
            , fHigh = rgb255 35 25 66
            , fMed = rgb255 94 84 142
            , fLow = rgb255 190 149 196
            , fInv = rgb255 224 177 203
            , bHigh = rgb255 255 255 255
            , bMed = rgb255 94 84 142
            , bLow = rgb255 190 149 196
            , bInv = rgb255 159 134 192
            }



-}
type alias HRTheme =
    { background : Color
    , fHigh : Color
    , fMed : Color
    , fLow : Color
    , fInv : Color
    , bHigh : Color
    , bMed : Color
    , bLow : Color
    , bInv : Color
    }


{-| Converts a Hundred Rabbits Theme to an XML string
so that it can be downloaded to the user as a file.

    import File.Download as Download
    import HRTheme

    save : Theme -> Cmd msg
    save theme =
        Download.string "theme.svg" "image/svg+xml" (HRTheme.toXmlString theme)

-}
toXmlString : HRTheme -> String
toXmlString theme =
    """
        <svg width="96px" height="64px" xmlns="http://www.w3.org/2000/svg" baseProfile="full" version="1.1">
        <rect width='96' height='64'  id='background' fill='""" ++ colorToHex theme.background ++ """'></rect>
        <!-- Foreground -->
        <circle cx='24' cy='24' r='8' id='f_high' fill='""" ++ colorToHex theme.fHigh ++ """'></circle>
        <circle cx='40' cy='24' r='8' id='f_med' fill='""" ++ colorToHex theme.fMed ++ """'></circle>
        <circle cx='56' cy='24' r='8' id='f_low' fill='""" ++ colorToHex theme.fLow ++ """'></circle>
        <circle cx='72' cy='24' r='8' id='f_inv' fill='""" ++ colorToHex theme.fInv ++ """'></circle>
        <!-- Background -->
        <circle cx='24' cy='40' r='8' id='b_high' fill='""" ++ colorToHex theme.bHigh ++ """'></circle>
        <circle cx='40' cy='40' r='8' id='b_med' fill='""" ++ colorToHex theme.bMed ++ """'></circle>
        <circle cx='56' cy='40' r='8' id='b_low' fill='""" ++ colorToHex theme.bLow ++ """'></circle>
        <circle cx='72' cy='40' r='8' id='b_inv' fill='""" ++ colorToHex theme.bInv ++ """'></circle>
    </svg>
    """










{-| Decodes a Hundred Rabbits theme SVG file/string into a
usable type in Elm.


    import Xml.Decode as XD -- from ymtszw/elm-xml-decode
    import HRTheme

    hundredRabbitsTheme = XD.run HRTheme.decoder xmlString


Alongside the String-based errors the XML decoder might generate,
this decoder will generate errors specific to Hundred Rabbits themes:

- Missing ID - eg. `The ID 'f_low' was not found.`
- Invalid color hex - eg. `The color at ID 'f_high' is not a valid hex color.`


-}
decoder : XD.Decoder HRTheme
decoder =
    backendDataDecoder
        |> XD.andThen attemptThemeConv


{-| Attempts to convert the raw data from the XML
decode into an actual theme.
-}
attemptThemeConv : BackendData -> XD.Decoder HRTheme
attemptThemeConv data =
    let
        themeConvAttempt = makeTheme data
    in
        case themeConvAttempt of
            Ok t -> succeed t
            Err r ->
                case r of
                    IDNotFound id ->
                        fail <| "The ID '" ++ id ++ "' was not found."
                        
                    InvalidColor id ->
                        fail <| "The color at ID '" ++ id ++ "' is not a valid hex color."


type alias BackendData = (List (String, String))

{-| Decodes the base SVG into data that can be
verified later in the decoding process
-}
backendDataDecoder : XD.Decoder BackendData
backendDataDecoder =
    map2 List.append 
        (path ["rect"] (list shapeDecoder))
        (path ["circle"] (list shapeDecoder))


{-| Internal function that decodes a single shape.
-}
shapeDecoder : XD.Decoder (String, String)
shapeDecoder = 
    map2 Tuple.pair
        (stringAttr "id")
        (stringAttr "fill")





{-| Errors that can occur during converting a
BackendData to a HRTheme.
-}
type Error
    = IDNotFound String
    | InvalidColor String




{-| Internal function that represents the part of the decoder that takes the raw
data from the XML, validates it and packs it into a working theme.
-}
makeTheme : BackendData -> Result Error HRTheme
makeTheme list =
    let 
        -- the list of all the IDs we need
        diffComparison = Dict.fromList
            [ ("background", "")
            , ("f_high", "")
            , ("f_med", "")
            , ("f_low", "")
            , ("f_inv", "")
            , ("b_high", "")
            , ("b_med", "")
            , ("b_low", "")
            , ("b_high", "")
            , ("b_inv", "")
            ]

        hasError d =
            d
            |> Dict.toList
            |> List.head

        dictConv = Dict.fromList list
        
        idCheck = Dict.diff diffComparison dictConv 

        colorConv = list
            |> Dict.fromList
            |> Dict.map convertColor
            |> Dict.filter (\_ v -> v /= Nothing)
            |> Dict.map withDefaultColorDict

        colorCheck = Dict.diff diffComparison colorConv


    in
        case hasError idCheck of
            Just e -> Err ( IDNotFound ( Tuple.first e ))
            Nothing ->  
        
                case hasError colorCheck of
                    Just e -> Err ( InvalidColor ( Tuple.first e ))
                    Nothing -> Ok { background = withDefaultColor <| Dict.get "background" colorConv
                                , fHigh = withDefaultColor <| Dict.get "f_high" colorConv
                                , fMed = withDefaultColor <| Dict.get "f_med" colorConv
                                , fLow = withDefaultColor <| Dict.get "f_low" colorConv
                                , fInv = withDefaultColor <| Dict.get "f_inv" colorConv
                                , bHigh = withDefaultColor <| Dict.get "b_high" colorConv
                                , bMed = withDefaultColor <| Dict.get "b_med" colorConv
                                , bLow = withDefaultColor <| Dict.get "b_low" colorConv
                                , bInv = withDefaultColor <| Dict.get "b_inv" colorConv
                                }


{-| Takes an item preliminary XML decode dict
and tries to return a SolidColor type from the color portion.
-}
convertColor : String -> String -> Maybe Color
convertColor _ color =
    color
    |> hexToColor
    |> Result.toMaybe


{-| At this step, we can assume that all the colors in the
dictonary are not Nothing, so let's unwrap the Maybe.
-}
withDefaultColor : Maybe Color -> Color
withDefaultColor color =
    case color of
        Nothing -> Color.black
        Just c -> c


{-| withDefaultColor, but with a key argument for iterating with a dict.
-}
withDefaultColorDict : String -> Maybe Color -> Color
withDefaultColorDict _ color = withDefaultColor color






{-| Generates an `Svg` of the theme in the same way that
100r theme files look like.

    HRTheme.toSvgImage theme
-}
toSvgImage : HRTheme -> Svg msg
toSvgImage theme =
    svg
        [ width "96px"
        , height "64px"
        , xmlBase "http://www.w3.org/2000/svg"
        , baseProfile "full"
        , version "1.1"
        ]
        [ bgRect theme
        , paletteCircle "f_high" theme.fHigh 24 24
        , paletteCircle "f_med" theme.fMed 40 24
        , paletteCircle "f_low" theme.fLow 56 24
        , paletteCircle "f_inv" theme.fInv 72 24
        , paletteCircle "b_high" theme.bHigh 24 40
        , paletteCircle "b_med" theme.bMed 40 40
        , paletteCircle "b_low" theme.bLow 56 40
        , paletteCircle "b_inv" theme.bInv 72 40
        ]


-- <rect width='96' height='64' id='background' fill='#171717'></rect>
bgRect : HRTheme -> Svg msg
bgRect theme =
    rect
        [ width "96"
        , height "64"
        , id "background"
        , fill <| colorToHex theme.background
        ]
        []

--<circle cx='24' cy='40' r='8' id='b_high' fill='#373737'></circle>
paletteCircle : String -> Color -> Int -> Int -> Svg msg
paletteCircle idStr col xPos yPos =
    circle
        [ cx <| String.fromInt xPos
        , cy <| String.fromInt yPos
        , r "8"
        , id idStr
        , fill <| colorToHex col
        ]
        []