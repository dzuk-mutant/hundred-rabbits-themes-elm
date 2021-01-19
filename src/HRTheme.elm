module HRTheme exposing (HRTheme, decoder)

{-| Decode theme files and use themes that conform to the
[Hundred Rabbits theme framework](https://github.com/hundredrabbits/Themes).

Because it would be impractical for `HRTheme`,
there is no constructor function, you just manually create the record.

@docs HRTheme, decoder
-}

import Color exposing (Color)
import Color.Convert exposing (hexToColor)
import Dict
import Xml.Decode as XD exposing (fail, list, map2, path, stringAttr, succeed)



{-| A color theme that uses the Hundred Rabbits theme spec.

Because there's no central Color type in elm, you may have
to convert these colours to another type when you get them
to be usable.

The Color types used in this record are from `avh4/elm-color`.
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















{-| Decodes a Hundred Rabbits theme SVG file/string into a
usable type in Elm.

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
