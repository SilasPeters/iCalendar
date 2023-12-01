module DateTime where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import Data.Foldable
import Control.Monad ( replicateM )

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)
instance Show DateTime where show dt = show (date dt) ++ "T" ++ show (time dt) ++ if utc dt then "Z" else ""

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)
instance Show Date where show d = show (year d) ++ show (month d) ++ show (day d)

makeLength :: Int -> String -> String
makeLength n s = replicate (n - length s) '0' ++ s

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)
instance Show Year where show = makeLength 4.show.runYear
instance Show Month where show = makeLength 2.show.runMonth
instance Show Day where show = makeLength 2.show.runDay

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)
instance Show Time where show t = show (hour t) ++ show (minute t) ++ show (second t)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)
instance Show Hour where show = makeLength 2.show.runHour
instance Show Minute where show = makeLength 2.show.runMinute
instance Show Second where show = makeLength 2.show.runSecond


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


-- Exercise 1 never gonna give
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseTimeUtc

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseYear :: Parser Char Year
parseYear = Year <$> parseInt 4

parseMonth :: Parser Char Month
parseMonth = Month <$> parseInt 2

parseDay :: Parser Char Day
parseDay = Day <$> parseInt 2

parseHour :: Parser Char Hour
parseHour = Hour <$> parseInt 2

parseMinute :: Parser Char Minute
parseMinute = Minute <$> parseInt 2

parseSecond :: Parser Char Second
parseSecond = Second <$> parseInt 2

parseTimeUtc :: Parser Char Bool
parseTimeUtc = option (True <$ symbol 'Z') False

parseDateSep :: Parser Char Char
parseDateSep = symbol 'T'

parseNDigits :: Int -> Parser Char [Int]
parseNDigits = flip replicateM newdigit

parseInt :: Int -> Parser Char Int
parseInt n = foldl1 ((+) . (*10)) <$> parseNDigits n

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p as = fst <$> find (null . snd) (parse p as)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = show

-- Exercise 4
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt = checkDate (date dt) && checkTime (time dt)

checkDate :: Date -> Bool
checkDate date = checkMonthAndDay date && checkYear (year date)

checkMonthAndDay :: Date -> Bool
checkMonthAndDay date = let d = runDay (day date) in
  case runMonth(month date) of
    1  -> 0 < d && d < 32
    2  -> 0 < d && d < (if isLeap (year date) then 30 else 29)
    3  -> 0 < d && d < 32
    4  -> 0 < d && d < 31
    5  -> 0 < d && d < 32
    6  -> 0 < d && d < 31
    7  -> 0 < d && d < 32
    8  -> 0 < d && d < 32
    9  -> 0 < d && d < 31
    10 -> 0 < d && d < 32
    11 -> 0 < d && d < 31
    12 -> 0 < d && d < 32
    _ -> False

checkYear :: Year -> Bool
checkYear = (\y -> y > 999 && y < 10000) . runYear

checkTime :: Time -> Bool
checkTime t = runHour(hour t) < 24 && runMinute (minute t) < 60 && runSecond (second t) < 60

isLeap :: Year -> Bool
isLeap year = let y = runYear year
                  divisable4   = y `mod` 4   == 0
                  divisable100 = y `mod` 100 == 0
                  divisable400 = y `mod` 400 == 0
               in divisable4 && (not divisable100 || divisable400)

