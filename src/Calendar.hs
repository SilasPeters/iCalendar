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

newtype ProdId = ProdId Text deriving (Eq, Ord, Show)

data Event = Event { getDtStamp     :: DtStamp
                   , getUid         :: Uid
                   , getDtStart     :: DtStart
                   , getDtEnd       :: DtEnd
                   , getDescription :: Maybe Description
                   , getSummary     :: Maybe Summary
                   , getLocation    :: Maybe Location }
    deriving (Eq, Ord, Show)

-- Define newtype's to enforce type checks
newtype DtStamp     = DtStamp DateTime deriving (Eq, Ord, Show)
newtype Uid         = Uid Text         deriving (Eq, Ord, Show)
newtype DtStart     = DtStart DateTime deriving (Eq, Ord, Show)
newtype DtEnd       = DtEnd DateTime   deriving (Eq, Ord, Show)
newtype Description = Description Text deriving (Eq, Ord, Show)
newtype Summary     = Summary Text     deriving (Eq, Ord, Show)
newtype Location    = Location Text    deriving (Eq, Ord, Show)

-- Since 'text' is not just a string, we would like to enforce a typecheck
-- This way, when we define functions which are only intended for 'text's,
-- we can require a Text to be provided instead of a context-dependend String
newtype Text = Text String deriving (Eq, Ord, Show)

-- Exercise 7
newtype Token = Token String deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
