module Data.Logging where

import Data.Time.Clock
import Data.List

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Read, Eq)

instance Show EventSource where
    show (Internal comp _)    = "Internal[" ++ comp ++ "]"
    show (External uri _)     = "External[" ++ uri ++ "]"
    show (Combined xs)        = "Combined[" ++ (intercalate "," $ map show xs) ++ "]"
    show (Unknown)            = "Unknown"

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Read, Eq)

instance Show LogMessage where
    show (LogMessage s m _ _ l) = "[" ++ show l ++ "]" ++ " " ++ show s ++ ": " ++ m

instance Ord LogMessage where
    (<=) lhs rhs = case (lmHiddenFlag lhs, lmHiddenFlag rhs) of
        (False, True) -> False
        (True, False) -> True
        _             -> case (lmLogLevel lhs /= lmLogLevel rhs) of
                            True -> lmLogLevel lhs < lmLogLevel rhs
                            _    -> lmTimestamp lhs <= lmTimestamp rhs

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

-- | Change log level operator
($=) :: LogMessage -> LogLevel -> LogMessage
($=) m l = m { lmLogLevel = l }


-- | EventSource "combinator"
(@@) :: EventSource -> EventSource -> EventSource
(@@) (Combined ls) (Combined rs) = Combined $ ls ++ rs
(@@) l@(Combined _) rs           = l @@ (Combined $ rs:[])
(@@) ls r@(Combined _)           = (Combined $ ls:[]) @@ r
(@@) l r                         = Combined $ l:r:[]

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) = undefined

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter  = undefined
