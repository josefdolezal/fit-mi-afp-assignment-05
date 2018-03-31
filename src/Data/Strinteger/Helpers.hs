module Data.Strinteger.Helpers where

import Data.Map as M

zero = "zero" -- zero is very special value

units :: [(Integer, String)]
units = [ ( 0, zero)
        , ( 1, "one")
        , ( 2, "two")
        , ( 3, "three")
        , ( 4, "four")
        , ( 5, "five")
        , ( 6, "six")
        , ( 7, "seven")
        , ( 8, "eight")
        , ( 9, "nine")
        , (10, "ten")
        , (11, "eleven") -- if bored, watch: https://www.youtube.com/watch?v=NMS2VnDveP8
        , (12, "twelve")
        , (13, "thirteen")
        , (14, "fourteen")
        , (15, "fifteen")
        , (16, "sixteen")
        , (17, "seventeen")
        , (18, "eighteen")
        , (19, "nineteen")
        ]

tens :: [(Integer, String)]
tens = [ (1, "ten")
       , (2, "twenty")
       , (3, "thirty")
       , (4, "forty")
       , (5, "fifty")
       , (6, "sixty")
       , (7, "seventy")
       , (8, "eighty")
       , (9, "ninety")
       ]

scales :: [(Integer, String)]
scales = [ (  2, "hundred")
         , (  3, "thousand")
         , (  6, "million")
         , (  9, "billion")
         , ( 12, "trillion")
         , ( 15, "quadrillion")
         , ( 18, "quintillion")
         , ( 21, "sextillion")
         , ( 24, "septillion")
         , ( 27, "octillion")
         , ( 30, "nonillion")
         , ( 33, "decillion")
         , ( 36, "undecillion")
         , ( 39, "duodecillion")
         , ( 42, "tredecillion")
         , ( 45, "quattuordecillion")
         , ( 48, "quindecillion")
         , ( 51, "sexdecillion")
         , ( 54, "septendecillion")
         , ( 57, "octodecillion")
         , ( 60, "novemdecillion")
         , ( 63, "vigintillion")
         ]

highestPossible = 10^64 - 1

-- | Map where key is numword part and value is the tuple (scale, value)
word2numMap :: M.Map String (Integer, Integer)
word2numMap = unions [numunits, numtens, numscales]
             where
               numunits  = M.fromList [(str, ( 1, value)) | (value, str) <- units]
               numtens   = M.fromList [(str, (10, value)) | (value, str) <- tens]
               numscales = M.fromList [(str, (10^scale, 0)) | (scale, str) <- scales]

-- | Translate word to (scale, value) tuple if defined
word2num :: String -> Maybe (Integer, Integer)
word2num k = M.lookup k word2numMap

-- | Map where key is (scale, value) tuple and numword is the value (inverse of word2numMap)
num2wordMap :: M.Map (Integer, Integer) String
num2wordMap = unions [wordunits, wordtens, wordscales]
             where
               wordunits  = M.fromList [(( 1, value), str) | (value, str) <- units]
               wordtens   = M.fromList [((10, value), str) | (value, str) <- tens]
               wordscales = M.fromList [((10^scale, 0), str) | (scale, str) <- scales]

-- | Translate scale and value to word if defined
num2word :: Integer -> Integer -> Maybe String
num2word scale value = M.lookup (scale, value) num2wordMap

separator = " " -- used between separate numerals
separatorTens = "-" -- used to connect tens and units: sixty-six
negativePrefix = "minus"
messageBadNumeral numeral = "Illegal English numeral: " ++ "'" ++ numeral ++ "'"
messageBadInteger integer = "Uncovertable Integer: " ++ show integer
