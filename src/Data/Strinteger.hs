module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate)
import Data.List.Split (splitOn)

-- Use Data.Strinteger.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.Strinteger.Helpers as SH

-- | Strinteger type (wrapper) for English numerals
newtype Strinteger = Strinteger String
                   deriving (Show, Read)

instance Bounded Strinteger where
   maxBound = pack SH.highestPossible
   minBound = negate maxBound

   -- | Pack Integer into Strinteger (English numeral string)
pack :: Integer -> Strinteger
pack integer = Strinteger $ fromMaybe err (integer2EngNumeral integer)
               where
                 err = error $ SH.messageBadInteger integer

-- | Unpack Strinteger (English numeral string) to Integer
unpack :: Strinteger -> Integer
unpack (Strinteger numeral) = fromMaybe err (engNumeral2Integer numeral)
                              where
                                err = error $ SH.messageBadNumeral numeral

-- | Translate Integer to String (if possible)
integer2EngNumeral :: Integer -> Maybe String
integer2EngNumeral n
    | n < 0     = (\x -> SH.negativePrefix ++ SH.separator ++ x) <$> integer2EngNumeral (-n)
    | n == 0    = Just SH.zero
    | otherwise = Just $ intercalate SH.separator (translateNumScales n $ reverse SH.scales)

translateNumUnits :: Integer -> [String]
translateNumUnits 0 = []
translateNumUnits n = (requireWord 1 n):[]

translateNumTens :: Integer -> [String]
translateNumTens n
    | n < 20    = translateNumUnits n
    | otherwise = (flatten $ tens : units):[]
        where tens    = requireWord 10 $ n `div` 10
              units   = translateNumUnits $ n `mod` 10
              flatten = intercalate SH.separatorTens

translateNumScales :: Integer -> [(Integer, String)] -> [String]
translateNumScales n ((d, t):xs)
    | n < 100   = translateNumTens n
    | otherwise = case (n `div` 10^d) of
        0 -> translateNumScales n xs
        x -> (translateNumScales x xs) ++ t : (translateNumScales (n `mod` 10^d) xs)
translateNumScales n _ = translateNumTens n

requireWord :: Integer -> Integer -> String
requireWord s n = case (SH.num2word s n) of
    Just w  -> w
    Nothing -> error $ SH.messageBadInteger n

-- | Translate String to Integer (if possible)
-- TODO: implement String->Integer translation
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer s = splitOn SH.separator 

translateLitUnit :: String -> Integer
translateLitUnit w = requireNumber

translateLitTens :: String -> Maybe Integer
translateLitTens w = case (splitOn SH.separatorTens w) of
    (tens:units:[]) -> word2num tens
    (tens:[])       ->

requireNumber :: String -> Integer
requireNumber w = case (SH.word2num w) of
    Just (s, n) = 10^s * n
    Nothing     = $ SH.messageBadNumeral w

-- TODO: implement Strinteger instances of Num, Ord, Eq, Enum, Real, and Integral
instance Eq Strinteger where
    (==) l r = (unpack l) == (unpack r)

instance Ord Strinteger where
    compare = undefined

instance Num Strinteger where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum Strinteger where
    toEnum = undefined
    fromEnum = undefined

instance Real Strinteger where
    toRational = undefined

instance Integral Strinteger where
    quotRem = undefined
    toInteger = undefined
