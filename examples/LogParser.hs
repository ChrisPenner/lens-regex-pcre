{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LogParser where

import Control.Lens
import Control.Lens.Regex
import Text.RawString.QQ
import Data.Text as T
import Text.Read

logs :: Text
logs =[r|<Error> <App> Config-file not found
<Error> <Kafka> No network access
<Info> <App> Waldo logged in
<Warn> <Kafka> 90% of disk space used
<Info> <App> Carmen logged out
<Error> <Kafka> Something really bad happened|]

-- define types to represent our log messages
data LogLevel = Info | Warn | Error deriving (Show, Read, Eq)
data LogService = Kafka | App deriving (Show, Read, Eq)
data Log =
    Log
    { logLevel :: LogLevel
    , logService :: LogService
    , logText  :: Text
    } deriving Show

-- Our parser is just a prism from a list of groups to our type!
_Log :: Prism' [Text] Log
_Log = prism' toGroups toLog
  where
    toGroups (Log level service msgText) = [pack (show level), pack (show service), msgText]
    toLog [levelText, serviceText, msgText] = do
        Log <$> readMaybe (unpack levelText)
            <*> readMaybe (unpack serviceText)
            <*> pure msgText
    toLog _ = Nothing

logPattern :: Regex
logPattern = [rx|<([\w]+)> <(\w+)> (.*)|]

-- Now you're ready to hack'n'slash!
allInfos :: [Log]
allInfos = logs ^.. regex logPattern . groups . _Log . filtered ((== Info) . logLevel)
-- [ Log {logLevel = Info, logService = App, logText = "Waldo logged in"}
-- , Log {logLevel = Info, logService = App, logText = "Carmen logged out"}]

allKafkaErrorMessages :: [Text]
allKafkaErrorMessages =
    logs
    ^.. regex logPattern
    . groups
    . _Log
    . filtered ((== Error) . logLevel)
    . filtered ((== Kafka) . logService)
    . to logText
-- ["No network access","Something really bad happened"]

downGradeErrors :: Text
downGradeErrors =
    logs & regex logPattern . groups . _Log . filtered ((== Error) . logLevel) %~ toWarning
  where
    toWarning (Log _level service txt) = Log Warn service (T.toUpper txt)
-- <Warn> <App> CONFIG-FILE NOT FOUND
-- <Warn> <Kafka> NO NETWORK ACCESS
-- <Info> <App> Waldo logged in
-- <Warn> <Kafka> 90% of disk space used
-- <Info> <App> Carmen logged out
-- <Warn> <Kafka> SOMETHING REALLY BAD HAPPENED
