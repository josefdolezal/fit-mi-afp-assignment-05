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
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) (Exact l) r                = l == r
(~~) (With l) (Combined r)      = l `elem` r
(~~) AnyInternal (Internal _ _) = True
(~~) AnyInternal (Combined rs)  = not $ null $ filter isInternal rs
    where isInternal (Internal _ _) = True
          isInternal _              = False
(~~) AnyExternal (External _ _) = True
(~~) AnyExternal (Combined rs) = not $ null $ filter isExternal rs
    where isExternal (External _ _) = True
          isExternal _              = False
(~~) Any _                      = True
(~~) (MatchAny ls) r            = any (~~ r) ls
(~~) (MatchAll ls) r            = all (~~ r) ls
(~~) _ _                        = False

-- | Specialized log list filter
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter m l h = (filterHidden h) . (filterLogLevel l) . (filterMatcher m)
    where filterHidden h   = filter (\x -> h == (lmHiddenFlag x))
          filterLogLevel l = filter (\x -> l <= (lmLogLevel x))
          filterMatcher m  = filter (\x -> m ~~ (lmSource x))
          