module CardParser (
    parseCard,
    stringToCard,
    Card (..),
    Rank (..),
    Suit (..),
    Enhancement (..),
    Edition (..),
    Seal (..),
) where

import Cards (Card (..), Edition (..), Enhancement (..), Rank (..), Seal (..), Suit (..))
import Control.Monad (void)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar)

type Parser = Parsec Void String

rankParser :: Parser Rank
rankParser =
    choice
        [ Two <$ char '2'
        , Three <$ char '3'
        , Four <$ char '4'
        , Five <$ char '5'
        , Six <$ char '6'
        , Seven <$ char '7'
        , Eight <$ char '8'
        , Nine <$ char '9'
        , Ten <$ char 't'
        , Jack <$ char 'j'
        , Queen <$ char 'q'
        , King <$ char 'k'
        , Ace <$ char 'a'
        ]
        <|> fail "invalid rank"

suitParser :: Parser Suit
suitParser =
    choice
        [ Spade <$ char 's'
        , Heart <$ char 'h'
        , Diamond <$ char 'd'
        , Club <$ char 'c'
        ]
        <|> fail "invalid suit"

quantityParser :: Parser Int
quantityParser = option 1 (read <$> some digitChar)

enhancementParser :: Parser Enhancement
enhancementParser =
    choice
        [ None <$ char 'n'
        , Bonus <$ char 'b'
        , Mult <$ char 'm'
        , Wild <$ char 'w'
        , Glass <$ char 'g'
        , Steel <$ char 's'
        , Gold <$ char 'y'
        , Lucky <$ char 'l'
        ]
        <?> "enhancement"

editionParser :: Parser Edition
editionParser =
    choice
        [ Base <$ char 'b'
        , Foil <$ char 'f'
        , Holographic <$ char 'h'
        , Polychrome <$ char 'p'
        ]
        <?> "edition"

sealParser :: Parser Seal
sealParser =
    choice
        [ NoneS <$ char 'n'
        , GoldS <$ char 'y'
        , Red <$ char 'r'
        , Blue <$ char 'b'
        , Purple <$ char 'p'
        ]
        <?> "seal"

parseCard :: Parser Card
parseCard = try parseNormalCard <|> try parseStoneCard <|> fail "Invalid card"

stringToCard :: String -> Maybe Card
stringToCard input = case parse (parseCard <* eof) "" (map toLower input) of
    Left _ -> Nothing
    Right card -> Just card

-- StoneCard: starts with 's' only (no rank/suit)
parseStoneCard :: Parser Card
parseStoneCard = do
    void (char 's')
    qty <- option 1 quantityParser
    edt <- optional editionParser
    sl <- optional sealParser
    pure $ StoneCard qty (fromMaybe Base edt) (fromMaybe NoneS sl)

-- NormalCard: [rank][suit][maybe quantity][maybe enh][maybe edt][maybe seal]
parseNormalCard :: Parser Card
parseNormalCard = do
    r <- rankParser
    s <- suitParser
    qty <- option 1 quantityParser
    enh <- optional enhancementParser
    edt <- optional editionParser
    sl <- optional sealParser
    pure $ NormalCard r s qty (fromMaybe None enh) (fromMaybe Base edt) (fromMaybe NoneS sl)
