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

{-
  datetime                          ::= date datesep time
  date                              ::= year month day
  time                              ::= hour minute second timeutc
  year                              ::= digit digit digit digit
  month, day, hour , minute, second ::= digit digit
  timeutc                           ::= ε | Z
  digit                             ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  datesep                           ::= T
 -}


-- Exercise 1 never
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseTimeUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = undefined

parseYear :: Parser Char Year
parseYear = undefined

parseMonth :: Parser Char Month
parseMonth = undefined

parseDay :: Parser Char Day
parseDay = undefined

parseHour :: Parser Char Hour
parseHour = toHour <$> newdigit <*> newdigit
    where
        toHour d1 d2 = Hour (10*d1 + d2)

parseMinute :: Parser Char Minute
parseMinute = toMin <$> newdigit <*> newdigit
    where 
        toMin d1 d2 = Minute $ twoDigits d1 d2

parseSecond :: Parser Char Second
parseSecond = toSec <$> newdigit <*> newdigit
    where
        toSec d1 d2 = Second $ twoDigits d1 d2

parseTimeUtc :: Parser Char Bool
parseTimeUtc = (== 'Z') <$> symbol 'Z'

parseDigit :: Parser Char Char
parseDigit = digit

parseDateSep :: Parser Char Char
parseDateSep = symbol 'T'


twoDigits :: Int -> Int -> Int
twoDigits d1 d2 = 10*d1 + d2


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
