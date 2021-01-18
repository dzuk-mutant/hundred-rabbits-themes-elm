module Theme exposing (Theme, Error, decoder)

import Dict exposing (Dict)
import SolidColor exposing (SolidColor)
import Xml.Decode as XD exposing (list, map, map2, path, stringAttr)

{- EXAMPLE THEME

<svg width="96px" height="64px" xmlns="http://www.w3.org/2000/svg" baseProfile="full" version="1.1">
  <rect width='96' height='64'  id='background' fill='#E0B1CB'></rect>
  <!-- Foreground -->
  <circle cx='24' cy='24' r='8' id='f_high' fill='#231942'></circle>
  <circle cx='40' cy='24' r='8' id='f_med' fill='#5E548E'></circle>
  <circle cx='56' cy='24' r='8' id='f_low' fill='#BE95C4'></circle>
  <circle cx='72' cy='24' r='8' id='f_inv' fill='#E0B1CB'></circle>
  <!-- Background -->
  <circle cx='24' cy='40' r='8' id='b_high' fill='#FFFFFF'></circle>
  <circle cx='40' cy='40' r='8' id='b_med' fill='#5E548E'></circle>
  <circle cx='56' cy='40' r='8' id='b_low' fill='#BE95C4'></circle>
  <circle cx='72' cy='40' r='8' id='b_inv' fill='#9F86C0'></circle>
</svg>

-}



{-| A color theme that adheres to the Hundred Rabbits theme spec.
-}
type alias Theme =
    { background : SolidColor
    , fHigh : SolidColor
    , fMed : SolidColor
    , fLow : SolidColor
    , fInv : SolidColor
    , bHigh : SolidColor
    , bMed : SolidColor
    , bLow : SolidColor
    , bInv : SolidColor
    }


{-| Errors that can occur during decoding a Hundred Rabbits theme.
-}
type Error
    = MalformedXML XD.Error
    | IDNotFound String
    | InvalidColor String

{-| Decodes a Hundred Rabbits theme SVG into a
usable Theme type in Elm.

The decoder may not succeed, so it returns a maybe.
-}
decoder : XD.Decoder (Result Error Theme)
decoder =
    map themeConstructor
        (path ["svg"] (list shapeDecoder))


{-| Internal function that decodes a single shape.
-}
shapeDecoder : XD.Decoder (String, String)
shapeDecoder = 
    map2 Tuple.pair
        (stringAttr "id")
        (stringAttr "fill")







{-| Takes an item preliminary XML decode dict
and tries to return a SolidColor type from the color portion.
-}
convertColor : String -> String -> Maybe SolidColor
convertColor _ color =
    color
    |> SolidColor.fromHex
    |> Result.toMaybe


{-| At this step, we can assume that all the colors in the
dictonary are not Nothing, so let's unwrap the Maybe.
-}
withDefaultColor : Maybe SolidColor -> SolidColor
withDefaultColor color =
    case color of
        Nothing -> SolidColor.fromRGB ( 0, 0, 0 )
        Just c -> c


{-| withDefaultColor, but with a key argument for iterating with a dict.
-}
withDefaultColorDict : String -> Maybe SolidColor -> SolidColor
withDefaultColorDict _ color = withDefaultColor color

{-| Internal function that represents the part of the decoder that takes the raw
data from the XML, validates it and packs it into a working theme.
-}
themeConstructor : List (String, String) -> Result Error Theme
themeConstructor list =
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
        
        idCheck = Dict.diff dictConv diffComparison

        colorConv = list
            |> Dict.fromList
            |> Dict.map convertColor
            |> Dict.filter (\_ v -> v /= Nothing)
            |> Dict.map withDefaultColorDict

        colorCheck = Dict.diff colorConv diffComparison


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


