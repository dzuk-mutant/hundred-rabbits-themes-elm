module Theme exposing (Theme, decoder)

import Dict exposing (Dict)
import SolidColor exposing (SolidColor)
import Xml.Decode as XD exposing (andMap, list, map2, path, stringAttr, succeed)


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



{-| Checks if an id is in a given preliminary dictionary from the XML decode.
-}
idIsIn : Dict String SolidColor -> String -> Bool -> Bool
idIsIn dict id prevResult =
    let
        result = List.any (\a -> Tuple.first a == id) (Dict.toList dict)
    in
        result && prevResult



themeConstructor : List (String, String) -> Maybe Theme
themeConstructor list =
    let
        -- get what worked first
        dict = 
            Dict.fromList list
            |> Dict.map convertColor
            |> Dict.filter (\_ v -> v /= Nothing)
            |> Dict.map withDefaultColorDict

        -- the list of all the IDs we need
        idList =
            [ "background"
            , "f_high"
            , "f_med"
            , "f_low"
            , "f_inv"
            , "b_high"
            , "b_med"
            , "b_low"
            , "b_high"
            , "b_inv"
            ]

        -- check if the XML tags we got have all the IDs we need
        hasItems =
            List.foldl (idIsIn dict) False idList

    in
        case hasItems of
            False -> Nothing
            True -> Just { background = withDefaultColor <| Dict.get "background" dict
                    , fHigh = withDefaultColor <| Dict.get "f_high" dict
                    , fMed = withDefaultColor <| Dict.get "f_med" dict
                    , fLow = withDefaultColor <| Dict.get "f_low" dict
                    , fInv = withDefaultColor <| Dict.get "f_inv" dict
                    , bHigh = withDefaultColor <| Dict.get "b_high" dict
                    , bMed = withDefaultColor <| Dict.get "b_med" dict
                    , bLow = withDefaultColor <| Dict.get "b_low" dict
                    , bInv = withDefaultColor <| Dict.get "b_inv" dict
                    }



{-| Decodes a Hundred Rabbits theme SVG into a
usable Theme type in Elm.

The decoder may not succeed, so it returns a maybe.
-}
decoder : XD.Decoder (Maybe Theme)
decoder =
    succeed themeConstructor
        |> andMap (path ["svg"] (list shapeDecoder))


{-| Internal function that decodes a single shape.
-}
shapeDecoder : XD.Decoder (String, String)
shapeDecoder = 
    map2 Tuple.pair
        (stringAttr "id")
        (stringAttr "fill")