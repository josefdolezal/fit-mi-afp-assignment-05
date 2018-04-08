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
    | not . inBounds $ n = Nothing
    | n < 0     = (\x -> SH.negativePrefix ++ SH.separator ++ x) <$> integer2EngNumeral (-n)
    | n == 0    = Just SH.zero
    | otherwise = intercalate SH.separator <$> (translateNumScales n $ reverse SH.scales)

translateNumUnits :: Integer -> Maybe [String]
translateNumUnits 0 = Just []
translateNumUnits n = (:[]) <$> SH.num2word 1 n

translateNumTens :: Integer -> Maybe [String]
translateNumTens n
    | n < 20    = translateNumUnits n
    | otherwise = flatten <$> (sequence $ tens:units:[])
        where tens    = (:[]) <$> (SH.num2word 10 $ n `div` 10)
              units   = translateNumUnits $ n `mod` 10
              flatten = (:[]) . (intercalate SH.separatorTens) . (foldl1 (++))

translateNumScales :: Integer -> [(Integer, String)] -> Maybe [String]
translateNumScales n ((d, t):xs)
    | n < 100   = translateNumTens n
    | otherwise = case (n `div` 10^d) of
        0 -> translateNumScales n xs
        x -> foldl1 (++) <$> sequence ds
            where ds = (translateNumScales x xs) : (Just [t]) : (translateNumScales (n `mod` 10^d) xs) : []
translateNumScales n _ = translateNumTens n

-- | Translate String to Integer (if possible)
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer s = translateLiterals $ splitOn SH.separator s

translateLiterals :: [String] -> Maybe Integer
translateLiterals xs = case (SH.negativePrefix `elem` xs) of
    True  -> (*(-1)) <$> (translateLiterals $ tail xs)
    False -> splitByMagnitude <$> mapM translateLitScales xs

translateLitScales :: String -> Maybe Integer
translateLitScales w = (singleNumber w) `fallback` (composedNumner w)
    where singleNumber w = (unwrap <$> (SH.word2num w))
          composedNumner = translateTens
          unwrap (s, n) = s * n

translateTens :: String -> Maybe Integer
translateTens s = case (splitOn SH.separatorTens s) of
    t@(x:y:[]) -> sum <$> mapM translateLitScales t
    _          -> Nothing

splitByMagnitude :: [Integer] -> Integer
splitByMagnitude []     = 0
splitByMagnitude (x:[]) = x
splitByMagnitude xs     = (max 1 $ splitByMagnitude ls) * highest + (splitByMagnitude $ tail rs)
    where (ls, rs) = span (< highest) xs
          highest  = maximum xs

-- Lazily returns second argument if first is Nothing
fallback :: Maybe a -> Maybe a -> Maybe a
fallback a@(Just _) _ = a
fallback _ r          = r

inBounds :: Integer -> Bool
inBounds n = n >= (-SH.highestPossible) && n <= SH.highestPossible

instance Eq Strinteger where
    (==) l r = (unpack l) == (unpack r)

instance Ord Strinteger where
    compare l r = compare (unpack l) (unpack r)

instance Num Strinteger where
    (+) l r = pack $ (unpack l) + (unpack r)
    (*) l r = pack $ (unpack l) * (unpack r)
    negate = pack . negate . unpack
    abs = pack . abs . unpack
    signum = pack . signum . unpack
    fromInteger = pack

instance Enum Strinteger where
    toEnum = pack . toInteger
    fromEnum = fromIntegral . unpack

instance Real Strinteger where
    toRational = toRational . unpack

instance Integral Strinteger where
    quotRem l r = (bin quot l r, bin rem l r)
        where bin op l r = pack $ (unpack l) `op` (unpack r)
    toInteger = unpack
