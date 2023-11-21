module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)

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
parseMonth = undefined

parseDay :: Parser Char Day
parseDay = undefined

parseHour :: Parser Char Hour
parseHour = Hour <$> parseTwoDigits

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseTwoDigits

parseSecond :: Parser Char Second
parseSecond = Second <$> parseTwoDigits

parseTimeUtc :: Parser Char Bool
parseTimeUtc = toUtc <$> option (symbol 'Z') 'N'
    where
        toUtc 'Z' = True
        toUtc 'N' = False

-- | waar is dit voor?
parseDigit :: Parser Char Char
parseDigit = digit -- TODO we don't use this?

parseDateSep :: Parser Char Char
parseDateSep = symbol 'T'



parseTwoDigits :: Parser Char Int
parseTwoDigits = (+) . (*10) <$> newdigit <*> newdigit

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
