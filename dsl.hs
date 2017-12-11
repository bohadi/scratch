{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- see https://www.schoolofhaskell.com/user/adinapoli/the-pragmatic-haskeller/the-pragmatic-haskeller-6

import Control.Lens hiding (noneOf)
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)

data Recipe = Recipe {
    _recipeName :: String
  , _ingredients :: [Ingredient]
  , _steps :: [Step]
} deriving Show

type Measure = String

data Ingredient = Ingredient {
    ingredientName :: String
  , quantity :: Int
  , measure :: Maybe Measure
} deriving Show

data Step = Step {
    _stepName :: String
  , _order :: Int
  , _stepDuration :: Maybe Duration
} deriving (Eq, Show)

instance Ord Step where
    compare s1 s2 = compare (_order s1) (_order s2)

data Duration = Duration {
    duration :: Int
  , durationMeasure :: Measure
} deriving (Show, Eq)

ws :: Parser String
ws = many (oneOf " ")

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q

measureP :: Parser (Maybe String)
measureP = (string "gr" *> (pure . Just $ "gr"))
       <|> (string "ml" *> (pure . Just $ "ml"))
       <|> (string "spoon" *> (pure . Just $ "spoon"))
       <|> (string "cup" *> (pure . Just $ "cup"))
       <|> (pure Nothing)

synSugar :: String -> Parser (Maybe String)
synSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing

ingredient :: Parser Ingredient
ingredient = do
    qt <- lexeme int
    ms <- lexeme measureP
    lexeme (synSugar "of")
    name <- lexeme stringLike
    lexeme (synSugar "and")
    string "\r\n"
    return $ Ingredient name qt ms

step :: Parser Step
step = do
    sn <- lexeme stringLike
    d <- optionMaybe durationP
    lexeme (synSugar "and")
    string "\r\n" <||> pure ""
    return $ Step sn 0 d

durationP :: Parser Duration
durationP = do
    lexeme (string "for")
    d <- lexeme int
    u <- lexeme durationUnit
    return $ Duration d u
  where durationUnit =
          string "seconds" <|> string "minutes" <|> string "hours"

makeLenses ''Step
makeLenses ''Recipe

correctOrder :: Recipe -> Recipe
correctOrder r = r { _steps = newSteps (_steps r) }
    where newSteps s = zipWith (over order) (const <$> [1..length s]) s

recipe :: Parser Recipe
recipe = do
    rn <- lexeme stringLike
    lexeme (synSugar "is made with") *> string "\r\n"
    i <- many1 ingredient
    many1 (string "\r\n")
    lexeme (string "prepared by") *> string "\r\n"
    s <- many1 step
    return $ Recipe rn i s

main :: IO ()
main = print $ correctOrder r where
    (Right r) = parse recipe "" example

example = unlines [
    "\"Ciambellone\" is made with\r",
    "    250 gr of \"Flour\"\r",
    "    250 gr of \"Sugar\"\r",
    "    130 ml of \"Sunflower Oil\"\r",
    "    130 ml of \"Water\"\r",
    "    3 \"Eggs\"\r",
    "\r",
    "  prepared by\r",
    "    \"Mixing everything\" and\r",
    "    \"Cooking in oven at 200 degrees\" for 40 minutes"
    ]
