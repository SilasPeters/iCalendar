module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import GHC.IO.Encoding (BufferCodec(setState))
import Data.Char (isUpper, isLetter)
--import qualified Data.List as words


{-
  event       ::= BEGIN:VEVENT crlf
                eventprop∗
                END:VEVENT crlf
  eventprop   ::= dtstamp | uid | dtstart | dtend | description | summary | location
  dtstamp     ::= DTSTAMP:     datetime crlf
  uid         ::= UID:         text     crlf
  dtstart     ::= DTSTART:     datetime crlf
  dtend       ::= DTEND:       datetime crlf
  description ::= DESCRIPTION: text     crlf
  summary     ::= SUMMARY:     text     crlf
  location    ::= LOCATION:    text     crlf


  calendar ::= BEGIN:VCALENDAR crlf
               calprop∗
               event∗
               END:VCALENDAR crlf
  calprop  ::= prodid | version
  prodid   ::= PRODID: text crlf
  version  ::= VERSION:2.0 crlf
-}

-- Exercise 6
data Calendar = Calendar { getProdId :: ProdId
                         -- , getVersion :: CalendarVersion -- Constant value
                         , getEvents :: [Event]}
    deriving (Eq, Ord, Show)

newtype ProdId = ProdId String deriving (Eq, Ord, Show)

data Event = Event { getDtStamp     :: DateTime
                   , getUid         :: String
                   , getDtStart     :: DateTime
                   , getDtEnd       :: DateTime
                   , getDescription :: Maybe String
                   , getSummary     :: Maybe String
                   , getLocation    :: Maybe String }
    deriving (Eq, Ord, Show)

data Property =      DtStamp      DateTime
                   | Uid          String
                   | DtStart      DateTime
                   | DtEnd        DateTime
                   | Description  String
                   | Summary      String
                   | Location     String

--
---- Define newtype's to enforce type checks --huh, waarom zou je niet gewoon DateTime & txt kunnen gebruiken?
--newtype DtStamp     = DtStamp DateTime   deriving (Eq, Ord, Show)
--newtype Uid         = Uid String         deriving (Eq, Ord, Show)
--newtype DtStart     = DtStart DateTime   deriving (Eq, Ord, Show)
--newtype DtEnd       = DtEnd DateTime     deriving (Eq, Ord, Show)
--newtype Description = Description String deriving (Eq, Ord, Show)
--newtype Summary     = Summary String     deriving (Eq, Ord, Show)
--newtype Location    = Location String    deriving (Eq, Ord, Show)

-- Since 'text' is not just a string, we would like to enforce a typecheck
-- This way, when we define functions which are only intended for 'text's,
-- we can require a Text to be provided instead of a context-dependend String
--newtype Text = Text String deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Value String | Prop String | DT DateTime deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = foldr plop [] <$> many parseToken

testScanCalendar :: IO()
testScanCalendar = do
  tekst <- readFile "examples//multiline.ics"
  print $ run scanCalendar tekst
  return ()

parseToken :: Parser Char Token
parseToken = choice [parseProp, DT <$> parseDateTime <* token "\n", parseValue]

plop :: Token -> [Token] -> [Token]
plop v1@(Value s1) (v2@(Value (' ':s2)):ts) = Value (s1++s2) : ts
plop t ts = t:ts

parseValue :: Parser Char Token
parseValue = Value <$> some (satisfy (/= '\n')) <* token "\n" 

parseProp :: Parser Char Token
parseProp = Prop <$> many (satisfy isUpper) <* symbol ':'




parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseEvent:: Parser Token Event
parseEvent = listToEvent <$> many parseProperty

parseProperty :: Parser Token Property
parseProperty = undefined

listToEvent:: [Property] -> Event
listToEvent ps = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
