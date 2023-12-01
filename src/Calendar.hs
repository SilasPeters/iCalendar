module Calendar where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)
import DateTime
import GHC.IO.Encoding (BufferCodec(setState))
import Data.Char (isUpper, isLetter)
import Data.Functor.Contravariant (Predicate(getPredicate))
import Data.Maybe

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
data Calendar = Calendar { getProdId :: String
                         , getVersion :: String
                         , getEvents :: [Event]}
    deriving (Eq, Ord, Show)

data Event = Event { getDtStamp     :: DateTime
                   , getUid         :: String
                   , getDtStart     :: DateTime
                   , getDtEnd       :: DateTime
                   , getDescription :: Maybe String
                   , getSummary     :: Maybe String
                   , getLocation    :: Maybe String }
    deriving (Eq, Ord, Show)


-- Exercise 7
data Token = Value String | Prop String | DT DateTime deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = foldr insertToken [] <$> many parseToken

insertToken :: Token -> [Token] -> [Token]
insertToken (Value s1) ((Value (' ':s2)):ts) = Value (s1++s2) : ts -- Concatenate multiline string
insertToken t ts = t:ts

parseToken :: Parser Char Token
parseToken = choice [parseProp, parseDT, parseValue] -- Order matters for 'run'

parseDT :: Parser Char Token
parseDT = DT <$> parseDateTime <* token "\r\n"

parseValue :: Parser Char Token
parseValue = Value <$> some (satisfy (/= '\r')) <* token "\r\n"

parseProp :: Parser Char Token
parseProp = Prop <$> many (satisfy isUpper) <* symbol ':'

data CalProp = ProdId String | Version String deriving (Show)

data Property = DtStamp      DateTime
              | Uid          String
              | DtStart      DateTime
              | DtEnd        DateTime
              | Description  String
              | Summary      String
              | Location     String
  deriving(Show)

isValue :: Token -> Bool
isValue (Value _) = True
isValue _ = False

isDT :: Token -> Bool
isDT (DT _) = True
isDT _ = False

getString :: Token -> String
getString (Value s) = s

getDate :: Token -> DateTime
getDate (DT dt) = dt

parseCalendar :: Parser Token Calendar
parseCalendar = toCal <$ symbol (Prop "BEGIN") <* symbol (Value "VCALENDAR")
                      <*> some parseCalProp <*> many parseEvent
                      <* symbol (Prop "END") <* symbol (Value "VCALENDAR")

-- Don't check just yet if the calendar is valid
toCal :: [CalProp] -> [Event] -> Calendar
toCal cps = foldr addEvent (foldl insertCalProp zeroCal cps)

insertCalProp :: Calendar -> CalProp -> Calendar
insertCalProp c cp = case cp of
  ProdId id -> c{getProdId = id}
  Version v -> c { getVersion = v }

addEvent :: Event -> Calendar -> Calendar
addEvent e c = c{getEvents = e:getEvents c}

zeroCal:: Calendar
zeroCal = Calendar "" "" []


parseCalProp :: Parser Token CalProp
parseCalProp  = choice [
    Version . getString <$ symbol (Prop "VERSION") <*> satisfy isValue
  , ProdId  . getString <$ symbol (Prop "PRODID")  <*> satisfy isValue
  ]


listToEvent:: [Property] -> Event
listToEvent = foldl insertProp zeroEvent


parseEvent:: Parser Token Event
parseEvent = listToEvent <$ symbol (Prop "BEGIN") <* symbol (Value "VEVENT") 
                         <*> many parseProperty 
                         <* symbol (Prop "END") <* symbol (Value "VEVENT")

testparseProperty tekst = fromJust $ run scanCalendar tekst

parseProperty :: Parser Token Property
parseProperty = choice [
    Uid         . getString <$ symbol (Prop "UID")         <*> satisfy isValue
  , Description . getString <$ symbol (Prop "DESCRIPTION") <*> satisfy isValue
  , Summary     . getString <$ symbol (Prop "SUMMARY")     <*> satisfy isValue
  , Location    . getString <$ symbol (Prop "LOCATION")    <*> satisfy isValue
  , DtStamp     . getDate   <$ symbol (Prop "DTSTAMP")     <*> satisfy isDT
  , DtStart     . getDate   <$ symbol (Prop "DTSTART")     <*> satisfy isDT
  , DtEnd       . getDate   <$ symbol (Prop "DTEND")       <*> satisfy isDT
  ]

zeroDate :: Date
zeroDate = Date (Year 0) (Month 0) (Day 0)

zeroTime :: Time
zeroTime = Time (Hour 0) (Minute 0) (Second 0)

zeroDT :: DateTime
zeroDT = DateTime zeroDate zeroTime False

zeroEvent :: Event
zeroEvent = Event zeroDT "" zeroDT zeroDT Nothing Nothing Nothing

getPropDate :: Property -> DateTime
getPropDate (DtStamp dt) = dt
getPropDate (DtStart dt) = dt
getPropDate (DtEnd dt) = dt

getPropString (Uid s) = s
getPropString (Description s) = s
getPropString (Summary s) = s
getPropString (Location s) = s


insertProp:: Event -> Property -> Event
insertProp e p = case p of
  DtStamp     dt -> e{getDtStamp     = getPropDate p}
  Uid          s -> e{getUid         = getPropString p}
  DtStart     dt -> e{getDtStart     = getPropDate p}
  DtEnd       dt -> e{getDtEnd       = getPropDate p}
  Description  s -> e{getDescription = Just $ getPropString p}
  Summary      s -> e{getSummary     = Just $ getPropString p}
  Location     s -> e{getLocation    = Just $ getPropString p}


recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined


--testScanCalendar :: IO()
--testScanCalendar = do
--  tekst <- readFile "examples//multiline.ics"
--  print $ run scanCalendar tekst
--  return ()


--testScan tekst = run scanCalendar tekst

--testparseCalendar :: IO()
--testparseCalendar = do
--  tekst <- readFile "examples//multiline.ics"
--  print $ run parseCalendar $ fromJust $ run scanCalendar tekst
--  return ()

--testCal :: [Char] -> Maybe Calendar
--testCal tekst = run parseCalendar $ fromJust $ run scanCalendar tekst

----mag ooit weg, als het werkt
---- TEST testParseCalProp "VERSION:3\n"    -> Just (Version "3")
--testParseCalProp txt= run parseCalProp $ fromJust $ run scanCalendar txt

----mag ooit weg, als het werkt
--testParseEvent :: [Char] -> Maybe Event 
--testParseEvent txt= run parseEvent $ fromJust $ run scanCalendar txt

