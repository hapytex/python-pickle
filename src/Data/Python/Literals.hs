{-# LANGUAGE BangPatterns, TupleSections #-}

module Data.Python.Literals where

import Data.Binary(Get, getWord8)
import Data.Binary.Get(isEmpty)
import Data.Bool(bool)
import Data.Word(Word8)

optUnderscore :: Get Word8
optUnderscore = do
  w <- getWord8
  case w of
    95 -> getWord8  -- we skip only once
    n -> pure n

readDigit' :: Word8 -> Get Word8
readDigit' 10 = pure 255
readDigit' n | 48 <= n && n <= 57 = pure (n-48)
             | 65 <= n && n <= 70 = pure (n-55)
             | 97 <= n && n <= 102 = pure (n-87)
             | otherwise = fail "Expecting a digit."

getDigit' :: Get Word8
getDigit' = optUnderscore >>= readDigit'

readDigit :: Word8 -> Word8 -> Get Word8
readDigit mx = go
  where go 255 = pure 255
        go d | d < mx = pure d
             | otherwise = fail "Expected a digit"

getDigit :: Word8 -> Get Word8
getDigit mx = getDigit' >>= readDigit mx

parseNum' :: Word8 -> Integer -> Get Integer
parseNum' rdx = go
  where go !n = do
          e <- isEmpty
          case e of
            True -> pure n
            False -> do
              w <- d
              case w of
                255 -> pure n
                _ -> go (r*n+fromIntegral w)
        d = getDigit rdx
        r = fromIntegral rdx

parseNum :: Word8 -> Get Integer
parseNum = (`parseNum'` 0)

withMode :: Word8 -> Get Integer
withMode  10 = pure 0  -- end of line
withMode  48 = parseNum  1  -- '0' (only zeros)
withMode  66 = parseNum  2  -- 'B'
withMode  79 = parseNum  8  -- 'O'
withMode  88 = parseNum 16  -- 'X'
withMode  95 = getWord8 >>= bool (fail "After zero, only more zeros are allowed") (parseNum 1) . (0 ==) -- '_' (underscore, no two consecutives, so only 0)
withMode  98 = parseNum  2  -- 'b'
withMode 111 = parseNum  8  -- 'o'
withMode 120 = parseNum 16  -- 'x'
withMode _ = fail "Invalid integer literal."

getSign :: Get (Integer -> Integer, Word8)
getSign = getWord8 >>= go
  where go 43 = go' (id,)  -- '+'
        go 45 = go' (negate,) -- '-'
        go d = (id,) <$> (readDigit 10 d >>= readDigit 10)
        go' = (`fmap` getDigit 10)

-- decimal digits always start with nonzero number
intParser :: Get Integer
intParser = do
  ~(s, w) <- getSign
  s <$> case w of
    255 -> fail "Empty integer"
    0 -> getWord8 >>= withMode
    _ -> parseNum' 10 (fromIntegral w)
