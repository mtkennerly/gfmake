module GFMake.Internal where

import Control.Applicative
  ( (<|>)
  )
import Data.List
  ( intercalate
  )
import Data.Maybe
  ( catMaybes
  , fromMaybe
  )

import Text.Parsec
  ( anyChar
  , char
  , endOfLine
  , many
  , many1
  , manyTill
  , noneOf
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
  | Header2 String
  | Header3 String
  | Header3B String
  | Header4 String
  | Header5 String
  | Option Int String  -- ^ Nesting level, then option phrase.
  | OptionDelim
  | Spacer
  | MarkupFlag
  | NoOp
  deriving (Eq, Show)

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
serializeElement (Option l e)  = "\n*" ++ show l ++ ". " ++ e
serializeElement OptionDelim   = "\n%"
serializeElement Spacer        = "\n"
serializeElement MarkupFlag    = ";format:gf-markup\n"
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
  <|> speechRule
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

noOpRule :: Parser Element
noOpRule = try $ do
  _ <- anyChar
  return NoOp
