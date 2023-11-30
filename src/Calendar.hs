module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime


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
newtype Token = Token String deriving (Eq, Ord, Show)

scanCalender :: Parser Char [Token]
scanCalender =  map Token . filter (not.null) .words <$> many anySymbol

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

parseEvent:: Parser Token Event
parseEvent = undefined

listToEvent:: [Property] -> Event
listToEvent ps = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalender s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
