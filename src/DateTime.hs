module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)

{- NOTE THAT THESE NAMES DO NOT PER SAY MATCH THE DATA TYPES
  datetime                          ::= date datesep time
  date                              ::= year month day
  time                              ::= hour minute second timeutc
  year                              ::= digit digit digit digit
  month, day, hour , minute, second ::= digit digit
  timeutc                           ::= ε | Z
  digit                             ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  datesep                           ::= T
 -}


-- Exercise 1 never gonna
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseTimeUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseYear :: Parser Char Year
parseYear = Year <$> (((+) . (*100) <$> parseTwoDigits) <*> parseTwoDigits)

parseMonth :: Parser Char Month
parseMonth = Month <$> parseTwoDigits

parseDay :: Parser Char Day
parseDay = Day <$> parseTwoDigits

parseHour :: Parser Char Hour
parseHour = Hour <$> parseTwoDigits

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseTwoDigits

parseSecond :: Parser Char Second
parseSecond = Second <$> parseTwoDigits

parseTimeUtc :: Parser Char Bool
parseTimeUtc = (== 'Z') <$> option (symbol 'Z') 'N'

parseDateSep :: Parser Char Char
parseDateSep = symbol 'T'



parseTwoDigits :: Parser Char Int
--parseTwoDigits = (+) . (*10) <$> newdigit <*> newdigit
parseTwoDigits = do
    base10 <- newdigit
    base1  <- newdigit
    return $ base10 * 10 + base1

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p s = case parse (some p) s of
        ((xs, _):_) -> Just $ last xs  -- If a non-empty list is returned, everything was parsed until there was nothing left to parse
        _           -> Nothing  -- If an empty list is returned, this indicates failure or that there was nothing to parse



-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
