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
data Calendar = Calendar
    deriving (Eq, Ord, Show)

data Event = Event
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined
