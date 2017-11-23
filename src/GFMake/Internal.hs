module GFMake.Internal where

import Control.Applicative
  ( liftA2
  )
import Data.List
  ( intercalate
  , isInfixOf
  , isPrefixOf
  , isSuffixOf
  )

import Data.List.Split
  ( splitOn
  )
import Data.String.Utils
  ( endswith
  , replace
  , split
  , strip
  )

data Element
  = Narration String
  | Speech String String  -- ^ Speaker name, then spoken words.
  | Cont String
  | Header2 String
  | Header3 String
  | Header3Cont String
  | Header3B String
  | Header4 String
  | Header4Anno String
  | Header5 String
  | Option Int String  -- ^ Nesting level, then option phrase.
  | OptionDelim
  | Spacer
  | MarkupFlag
  | NoOp
  deriving (Eq, Show)

squeezeElements :: [Element] -> [Element]
squeezeElements []                           = []
squeezeElements [x]                          = [x]
squeezeElements (Narration x:Cont y:xs)      = squeezeElements (Narration (x ++ " " ++ y) : xs)
squeezeElements (Speech who what:Cont y:xs)  = squeezeElements (Speech who (what ++ " " ++ y) : xs)
squeezeElements (x:Cont _:xs)                = x : squeezeElements xs
squeezeElements (Header3 x:Header3Cont y:xs) = squeezeElements (Header3 (x ++ " " ++ y) : xs)
squeezeElements (Header4Anno x:Header4 y:xs) = squeezeElements (Header4 (y ++ " / " ++ x) : xs)
squeezeElements (Header4 x:Header4Anno y:xs) = squeezeElements (Header4 (x ++ " / " ++ y) : xs)
squeezeElements (x:y:xs)                     = x : squeezeElements (y : xs)

padElements :: [Element] -> [Element]
padElements []                            = []
padElements [x]                           = [x]
padElements (Narration x:Speech sx sy:xs) = Narration x : Spacer : padElements (Speech sx sy : xs)
padElements (Speech sx sy:Narration y:xs) = Speech sx sy : Spacer : padElements (Narration y : xs)
padElements (OptionDelim:Option l o:xs)   = OptionDelim : padElements (Option l o : xs)
padElements (x:Option l o:xs)             = x : Spacer : padElements (Option l o : xs)
padElements (x:y:xs)
  | not (isHeader x) && isHeader y = x : Spacer : padElements (y : xs)
  | otherwise                      = x : padElements (y : xs)

parseLines :: [String] -> [Element]
parseLines = filter (/= NoOp) . map parseLine

parseLine :: String -> Element
parseLine line
  | isH2 l          = Header2 (h2Or3Text l)
  | isH3 l          = Header3 (h2Or3Text l)
  | isH3Cont l      = Header3Cont (h3ContText l)
  | isH3B l         = Header3B (h3bText l)
  | isH4 l          = Header4 (h4Text l)
  | isH4Anno l      = Header4Anno (h4Anno l)
  | isH5 l          = Header5 (h5Text l)
  | isOptionDelim l = OptionDelim
  | isOption l      = Option (optionLevel line) (optionText line)
  | isCont line     = Cont l
  | isNarration l   = Narration (strip (drop 3 l))
  | isSpeech l      = uncurry Speech (splitOnce ":" l)
  | otherwise       = NoOp
  where
    l = strip line

serializeElements :: [Element] -> String
serializeElements = concatMap serializeElement

serializeElement :: Element -> String
serializeElement (Header2 e)   = "\n==" ++ e ++ "=="
serializeElement (Header3 e)   = "\n===" ++ e ++ "==="
serializeElement (Header3B e)  = "\n===++" ++ e ++ "++==="
serializeElement (Header4 e)   = "\n====" ++ e ++ "===="
serializeElement (Header5 e)   = "\n=====" ++ e ++ "====="
serializeElement (Narration e) = "\n" ++ e
serializeElement (Speech n s)  = "\n| " ++ strip n ++ " | " ++ strip s ++ " |"
serializeElement (Option l e)  = "\n*" ++ show l ++ ". " ++ e
serializeElement OptionDelim   = "\n%"
serializeElement Spacer        = "\n"
serializeElement MarkupFlag    = ";format:gf-markup\n"
serializeElement _             = ""

isNarration :: String -> Bool
isNarration = (== "* -") . take 3

isSpeech :: String -> Bool
isSpeech = elem ':'

h2Or3Text :: String -> String
h2Or3Text = strip . (!! 1) . splitOn "---"

h3ContText :: String -> String
h3ContText = strip . drop 5 . reverse . drop 5 . reverse

h3bText :: String -> String
h3bText = drop 4 . reverse . drop 4 . reverse

h4Text :: String -> String
h4Text = strip . dropSepIfNotAnnotated . replace "#$" "|" . replace "$#" ""
  where
    dropSepIfNotAnnotated x
      | endswith "|" x = replace "|" "" x
      | otherwise      = x

h4Anno :: String -> String
h4Anno = strip . dropWhile (`elem` ['#', '$'])

h5Text :: String -> String
h5Text = strip . drop 1 . reverse . drop 1 . reverse

optionText :: String -> String
optionText = drop 7 . reverse . drop 1 . reverse

isH2 :: String -> Bool
isH2 = isPrefixOf "#$#$#   ["

isH3 :: String -> Bool
isH3 = isPrefixOf "#$# ["

isH3Cont :: String -> Bool
isH3Cont = isPrefixOf "#$#$#    "

isH3B :: String -> Bool
isH3B x = "\\_" `isPrefixOf` x && "_/" `isSuffixOf` x

isH4 :: String -> Bool
isH4 = isPrefixOf "$#  "

-- Identify h4 lines with some trailing text afterward, but ignore false
-- positives from the right side of h2 and h3.
isH4Anno :: String -> Bool
isH4Anno line = "#$#$#$#$#  " `isInfixOf` l || "$#$#$#$#$  " `isInfixOf` l && not (" $#$#$#$#$#$#$#" `isInfixOf` l)
  where
    l = strip line

isH5 :: String -> Bool
isH5 s = isPrefixOf "[ " s && isSuffixOf " ]" s

isOption :: String -> Bool
isOption l = any (`isPrefixOf` l) responsePrefixes

isOptionDelim :: String -> Bool
isOptionDelim = (== replicate 59 '-')

isCont :: String -> Bool
isCont = liftA2 (&&) (" " `isPrefixOf`) (not . isHeaderFluff)

isHeaderFluff :: String -> Bool
isHeaderFluff = ("_=======" `isInfixOf`)

responsePrefixes :: [String]
responsePrefixes = [m ++ b | m <- markers, b <- braces]
  where
    markers = [replicate n '-' | n <- [1..6]] ++ [replicate (6 - n) '-' ++ replicate n '=' | n <- [1..6]]
    braces  = ["(", "{"]

splitOnce :: String -> String -> (String, String)
splitOnce by xs = (head once, intercalate ":" $ tail once)
  where
    once = split by xs

optionLevel :: String -> Int
optionLevel s = countHyphens + countEquals * 2
  where
    marker       = takeWhile (`elem` ['-', '=', ' ']) s
    countHyphens = length $ filter (== '-') marker
    countEquals  = length $ filter (== '=') marker

isHeader :: Element -> Bool
isHeader (Header2 _)     = True
isHeader (Header3 _)     = True
isHeader (Header3B _)    = True
isHeader (Header3Cont _) = True
isHeader (Header4 _)     = True
isHeader (Header5 _)     = True
isHeader _               = False
