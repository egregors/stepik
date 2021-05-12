import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString m = time ++ ": " ++ level ++ ": " ++ msg
  where
    time = timeToString $ timestamp m
    level = logLevelToString $ logLevel m
    msg = message m