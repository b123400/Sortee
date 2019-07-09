{-# LANGUAGE FlexibleInstances #-}
module Data.String.Sortee
    ( between
    , Sortee(..)
    , chars
    ) where

-- Tasks are sorted by a string, inspired by Jira's Lexorank.
-- ref https://stackoverflow.com/a/49956113/343065
-- ref https://confluence.atlassian.com/jirakb/lexorank-management-779159218.html
-- ref https://www.youtube.com/watch?v=OjQv9xMoFbg

import Data.List ((!!), elemIndex)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.String (IsString, fromString)

newtype Sortee = Sortee { unSortee :: String } deriving (Show, Eq, Ord)

instance IsString Sortee where
    fromString = Sortee

instance IsString (Maybe Sortee) where
    fromString = Just . Sortee

-- | All the characters used in a sort string,
--   it's [0-9A-Za-z] sorted alphabetically
chars :: String
chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

middleChar :: Char
middleChar = chars !! ((length chars - 1) `div` 2)

empty :: Sortee
empty = Sortee ""

-- | Create a new sort string that is between the provided strings.
-- For example:
--
-- > between "a" "c" -- "b"
--
-- Laws:
--
-- > a < (between a b) < b
-- > (between Nothing a) < a
-- > (between a Nothing) > a
-- > (between a a) == Nothing
-- > (between (Just a) (Just b)) == (between (Just b) (Just a))
between :: Maybe Sortee -> Maybe Sortee -> Maybe Sortee
between Nothing Nothing = Just $ Sortee [middleChar]
between Nothing (Just a) = between' empty a
between (Just a) Nothing = between' a empty
between (Just a) (Just b) =
    if a > b
        then between' b a
        else between' a b

between' :: Sortee -> Sortee -> Maybe Sortee
between' (Sortee a) (Sortee b) = Sortee <$> betweenString a b

betweenString :: String -> String -> Maybe String
betweenString _ ('0': []) = Nothing
betweenString "" "" = Nothing

-- betweenString "a" "a"
betweenString (low:[]) (high:[])
  | low == high = Nothing

  | upperIndex <- high `elemIndex` chars
  , lowerIndex <- low `elemIndex` chars
  , upperIndex == ((+ 1) <$> lowerIndex) = Just [low, middleChar]

  | otherwise = do upperIndex <- high `elemIndex` chars
                   lowerIndex <- low `elemIndex` chars
                   Just $ [chars !! ((upperIndex + lowerIndex) `div` 2)]

betweenString "" (high:highs) = do upperIndex <- high `elemIndex` chars
                                   case upperIndex of
                                       1 -> Just $ [head chars, middleChar]
                                       0 -> (high :) <$> betweenString "" highs
                                       _ -> Just $ [chars !! (upperIndex `div` 2)]

betweenString (low:lows) "" = do lowerIndex <- low `elemIndex` chars
                                 if lowerIndex /= length chars - 1
                                     then Just $ [chars !! ((length chars + lowerIndex) `div` 2)]
                                     else if length lows == 0 then Just [low, middleChar]
                                     else (low :) <$> betweenString lows ""

betweenString (low: lows) (high:highs)
  -- betweenString "ab" "ac"
  | low == high = (low :) <$> (betweenString lows highs)

  -- betweenString "aa" "bx"
  | upperIndex <- high `elemIndex` chars
  , lowerIndex <- low `elemIndex` chars
  , upperIndex == ((+ 1) <$> lowerIndex) = do
      nextLowerIndex <- maybe (Just 0) (`elemIndex` chars) (listToMaybe lows)
      nextUpperIndex <- maybe (Just 0) (`elemIndex` chars) (listToMaybe highs)
    -- charsBetween (62 - 1) - 61 + 0 = 0
      let charsBetween = (length chars - 1) - nextLowerIndex + nextUpperIndex
      case (charsBetween, lows) of
        -- betweenString "az" ("b" / "b0")
        (0, (_:[])) -> Just (low: lows ++ [middleChar])
        -- betweenString "aza" "b0"
        (0, (nextLow:nextLows)) -> ([low, nextLow] ++) <$> betweenString nextLows ""
        -- betweenString "ax" "b0"
        _ | added <- nextLowerIndex + (upperMid charsBetween), added < length chars -> Just [low, chars !! added]
          | subtracted <- nextUpperIndex - (upperMid charsBetween), subtracted >= 0 -> Just [high, chars !! subtracted]

  -- betweenString "aa" "xx"
  | otherwise = do upperIndex <- high `elemIndex` chars
                   lowerIndex <- low `elemIndex` chars
                   Just $ [chars !! (upperMid $ upperIndex + lowerIndex)]

upperMid :: Int -> Int
upperMid x = fromIntegral $ ceiling $ (fromIntegral x) / 2
