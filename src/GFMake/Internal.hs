module GFMake.Internal where

import Control.Applicative
  ( (<|>)
  )
import Data.Char
  ( intToDigit
  )
import Data.List
  ( intercalate
  , groupBy
  )
import Data.List.Split
  ( splitOn
  )
import Data.Maybe
  ( catMaybes
  , fromMaybe
  )

import Text.Parsec
  ( anyChar
  , char
  , endOfLine
  , lookAhead
  , many
  , many1
  , manyTill
  , noneOf
  , notFollowedBy
  , oneOf
  , optionMaybe
  , parse
  , sepBy
  , spaces
  , string
  , try
  )
import Text.Parsec.String
  ( Parser
  )

data Element
  = Narration String
  | Speech String String  -- ^ Speaker name, then spoken words.
  | CondSpeech String [(String, String)]  -- ^ Name, condition descriptor, spoken words
  | Header2 String
  | Header3 String
  | Header3B String
  | Header4 String
  | Header5 String
  | Option Int String  -- ^ Nesting level, then option phrase.
  | OptionDelim
  | OptionSet String [Element]  -- ^ Option phrase, contained elements
  | Spacer
  | MarkupFlag
  | Comment String
  | NoOp
  deriving (Eq, Show)

padElements :: [Element] -> [Element]
padElements []                            = []
padElements [x]                           = [x]
padElements (Narration x:Speech sx sy:xs) = Narration x : Spacer : padElements (Speech sx sy : xs)
padElements (Speech sx sy:Narration y:xs) = Speech sx sy : Spacer : padElements (Narration y : xs)
padElements (Narration x:CondSpeech sx sy:xs) = Narration x : Spacer : padElements (CondSpeech sx sy : xs)
padElements (CondSpeech sx sy:Narration y:xs) = CondSpeech sx sy : Spacer : padElements (Narration y : xs)
padElements (OptionDelim:Option l o:xs)   = OptionDelim : padElements (Option l o : xs)
padElements (x:Option l o:xs)             = x : Spacer : padElements (Option l o : xs)
padElements (OptionSet o1 os1:OptionSet o2 os2:xs) = OptionSet o1 os1 : Spacer : padElements (OptionSet o2 os2 : xs)
padElements (Narration x:OptionSet o os:xs) = Narration x : Spacer : padElements (OptionSet o os : xs)
padElements (Speech x y:OptionSet o os:xs) = Speech x y : Spacer : padElements (OptionSet o os : xs)
padElements (CondSpeech x y:OptionSet o os:xs) = CondSpeech x y : Spacer : padElements (OptionSet o os : xs)
padElements (x:y:xs)
  | not (isHeader x) && isHeader y = x : Spacer : padElements (y : xs)
  | otherwise                      = x : padElements (y : xs)

constructOptionSets :: [Element] -> [Element]
constructOptionSets [] = []
constructOptionSets xs = concat [if odd n then group else makeSets 1 group | (n, group) <- zip [1..] groups]
  where
    groups :: [[Element]]
    groups = splitOn [OptionDelim] xs

    makeSets :: Int -> [Element] -> [Element]
    makeSets _ [] = []
    makeSets level group = do
      let byLevel = groupBy (\x y -> not (isOption level y)) group
      concatMap (makeSet level) byLevel

    makeSet :: Int -> [Element] -> [Element]
    makeSet _ [] = []
    makeSet _ [x] = [x]
    makeSet level (Option l e : group)
      | level == l = [OptionSet e (makeSets (level + 1) group)]
      | otherwise = makeSets (level + 1) (Option l e : group)
    makeSet level (x:y) = x : makeSet level y

serializeElements :: [Element] -> String
serializeElements = concatMap serializeElement

serializeElement :: Element -> String
serializeElement (Header2 e)   = "\n==" ++ e ++ "=="
serializeElement (Header3 e)   = "\n===" ++ e ++ "==="
serializeElement (Header3B e)  = "\n===++" ++ e ++ "++==="
serializeElement (Header4 e)   = "\n====" ++ e ++ "===="
serializeElement (Header5 e)   = "\n=====" ++ e ++ "====="
serializeElement (Narration e) = "\n" ++ e
serializeElement (Speech n s)  = "\n| " ++ n ++ " | " ++ s ++ " |"
serializeElement (CondSpeech _ []) = ""
serializeElement (CondSpeech n cs) = concatMap format namedChunks
  where
    chunks = chunked 15 cs
    namedChunks = if length chunks > 1
      then (n, head chunks) : map ((,) "[cont]") (tail chunks)
      else [(n, cs)]
    rowSpan xs = ['-', intToDigit (length xs)]
    pairs = intercalate "\n" . map (\(x, y) -> "|   " ++ x ++ " | " ++ y ++ " |")
    format (n', cs') =
      -- First line.
      "\n|" ++ rowSpan cs' ++ " " ++ n' ++ " | " ++ fst (head cs') ++ " | " ++ snd (head cs') ++ " |"
      -- Remaining lines.
      ++ (if length cs' > 1 then "\n" else "") ++ pairs (tail cs')
serializeElement (Option l e)  = "\n*" ++ show l ++ ". " ++ e
serializeElement OptionDelim   = ""
serializeElement (OptionSet x xs) = "\n=--" ++ x ++ "--=" ++ serializeElements xs ++ "\n=-="
serializeElement Spacer        = "\n"
serializeElement MarkupFlag    = ";format:gf-markup\n"
serializeElement (Comment e)   = "\n; " ++ e
serializeElement NoOp          = ""

responsePrefixes :: [String]
responsePrefixes = [m ++ b | m <- markers, b <- braces]
  where
    markers = [replicate n '-' | n <- [1..6]] ++ [replicate (6 - n) '-' ++ replicate n '=' | n <- [1..6]]
    braces  = ["(", "{"]

optionLevel :: String -> Int
optionLevel s = countHyphens + countEquals * 2
  where
    marker       = takeWhile (`elem` ['-', '=', ' ']) s
    countHyphens = length $ filter (== '-') marker
    countEquals  = length $ filter (== '=') marker

isHeader :: Element -> Bool
isHeader (Header2 _)  = True
isHeader (Header3 _)  = True
isHeader (Header3B _) = True
isHeader (Header4 _)  = True
isHeader (Header5 _)  = True
isHeader _            = False

isOption :: Int -> Element -> Bool
isOption level (Option l _) = level == l
isOption _ _ = False

strip :: String -> String
strip = reverse . lstrip . reverse . lstrip
  where
    lstrip = dropWhile (`elem` " \n\r\t")

parseElements :: String -> [Element]
parseElements s = case parse elemsRule "(script)" s of
  Right xs -> filter (/= NoOp) xs
  Left _   -> []

elemsRule :: Parser [Element]
elemsRule = many $
  h2Rule
  <|> h3Rule
  <|> h3BRule
  <|> h4Rule
  <|> h5Rule
  <|> optionDelimRule
  <|> optionRule
  <|> narrationRule
  <|> condSpeechRule
  <|> speechRule
  <|> commentRule
  <|> noOpRule

h2Rule :: Parser Element
h2Rule = try $ do
  _ <- string "#$#$# "
  _ <- manyTill anyChar (string "---")
  t <- manyTill anyChar (string "---")
  return $ Header2 $ strip t

h3Rule :: Parser Element
h3Rule = try $ do
  _ <- string "#$# "
  _ <- manyTill anyChar (string "---")
  t <- manyTill anyChar (string "---")
  _ <- manyTill anyChar endOfLine
  t2 <- optionMaybe (try $ string "#$#$#" >> manyTill (noneOf "#$") (string "#$#$#"))
  let
    t' = strip t
    t2' = strip (fromMaybe "" t2)
    sep = case t2 of
      Just _  -> " "
      Nothing -> ""
  return $ Header3 $ t' ++ sep ++ t2'

h3BRule :: Parser Element
h3BRule = try $ do
  _ <- spaces
  t <- string "\\_" >> manyTill anyChar (string "_/")
  return $ Header3B $ strip t

h4Rule :: Parser Element
h4Rule = try $ do
  _ <- many $ oneOf "#$"
  high <- optionMaybe (manyTill anyChar (char '\n'))
  t <- string "$#" >> manyTill anyChar (string "#$")
  mid <- optionMaybe (manyTill anyChar (char '\n'))
  _ <- many $ oneOf "#$"
  low <- optionMaybe (manyTill anyChar (char '\n'))
  let
    annotations = filter (not . null) $ map strip $ catMaybes [mid, high, low]
    t' = strip t
    sep = if null annotations
      then ""
      else " | "
  return $ Header4 $ t' ++ sep ++ intercalate " / " annotations

h5Rule :: Parser Element
h5Rule = try $ do
  _ <- endOfLine >> string "   ["
  t <- manyTill (noneOf "\r\n") (char ']')
  return $ Header5 $ strip t

optionDelimRule :: Parser Element
optionDelimRule = try $ do
  _ <- string (replicate 59 '-')
  return OptionDelim

optionRule :: Parser Element
optionRule = try $ do
  _ <- spaces
  level <- many1 $ oneOf "-="
  phrase <- try (char '(' >> manyTill anyChar (char ')')) <|> (char '{' >> manyTill anyChar (char '}'))
  return $ Option (optionLevel level) (strip phrase)

narrationRule :: Parser Element
narrationRule = try $ do
  _ <- string "* - "
  ts <- sepBy (many1 (noneOf "\r\n")) (try $ many1 endOfLine >> string "    ")
  return $ Narration (unwords $ map strip ts)

speechRule :: Parser Element
speechRule = try $ do
  name <- many1 $ noneOf ":\n\r"
  _ <- char ':'
  spoken <- sepBy (many1 (noneOf "\r\n")) (try $ many1 endOfLine >> string "      " >> many (char ' '))
  return $ Speech (strip name) (unwords $ map strip spoken)

condSpeechRule :: Parser Element
condSpeechRule = try $ do
  name <- many1 $ noneOf ":\n\r"
  _ <- char ':'
  pairs <- many1 $ do
    cond <- try $ do
      _ <- spaces
      special <- optionMaybe $ char '*'
      _ <- char '{'
      label <- manyTill anyChar (char '}')
      return $ case special of
        Just s  -> s : strip label
        Nothing -> strip label
    spoken <- sepBy (many1 (noneOf "\r\n")) (try $ many1 endOfLine >> string "      " >> spaces >> notFollowedBy (string "*{" <|> string "{"))
    return (cond, unwords $ map strip spoken)
  return $ CondSpeech (strip name) pairs

commentRule :: Parser Element
commentRule = try $ do
  _ <- many endOfLine >> lookAhead (noneOf " #$*-=:")
  t <- many1 $ noneOf "\r\n*#$_=:"
  _ <- endOfLine
  return $ Comment $ strip t

noOpRule :: Parser Element
noOpRule = try $ do
  _ <- anyChar
  return NoOp

chunked :: Int -> [a] -> [[a]]
chunked _ [] = []
chunked n xs = a : chunked n b
  where
    (a, b) = splitAt n xs
