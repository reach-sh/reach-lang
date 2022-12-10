-- Copied from https://hackage.haskell.org/package/sourcemap-0.1.7/docs/src/VLQ.html
-- BSD-3-Clause license
--
-- | Implements Base 64-encoded VLQ for 32-bit
-- integers. Implementation copied from
-- https://code.google.com/p/closure-compiler/source/browse/trunk/src/com/google/debugging/sourcemap/Base64VLQ.java
--

module Reach.VLQ
  (encode
  ,decode
  ,decode')
  where

import           Data.Bits hiding (shift)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Int
import           Data.List (elemIndex)
import           Data.Maybe
import           Data.Word
import           Prelude hiding ((>>))

-- | A Base64 VLQ digit can represent 5 bits, so it is base-32.
baseShift :: Int
baseShift = 5

-- | Base point.
base :: Int32
base = 1 << baseShift

-- | A mask of bits for a VLQ digit (11111), 31 decimal.
baseMask :: Int32
baseMask = base - 1

-- | The continuation bit is the 6th bit.
continuationBit :: Int32
continuationBit = base

-- | Converts from a two-complement value to a value where the sign
-- bit is is placed in the least significant bit.  For example, as
-- decimals:
--   1 becomes 2 (10 binary), -1 becomes 3 (11 binary)
--   2 becomes 4 (100 binary), -2 becomes 5 (101 binary)
toVlqSigned :: Int32 -> Int32
toVlqSigned value =
  if value < 0
     then ((-value) << 1) + 1
     else (value << 1) + 0

-- | Converts to a two-complement value from a value where the sign
-- bit is is placed in the least significant bit.  For example, as
-- decimals:
--   2 (10 binary) becomes 1, 3 (11 binary) becomes -1
--   4 (100 binary) becomes 2, 5 (101 binary) becomes -2
fromVlgSigned :: Int32 -> Int32
fromVlgSigned value =
  let value' = value >> 1
  in if (value & 1) == 1
        then -value'
        else value'

-- | Produces a ByteString containing a VLQ-encoded value of the given 32-bit integer.
encode :: Int32 -> ByteString
encode = B.map encodeBase64 . start where
  start 0 = B.singleton (fst (continue 0))
  start n = B.unfoldr go . toVlqSigned $ n

  go value
    | value <= 0 = Nothing
    | otherwise  = Just (continue value)

  continue value =
    let digit = value & baseMask
        value' = value >> baseShift
        digit' = if value' > 0
                    then digit .|. continuationBit
                    else digit
    in (fromIntegral digit',value')


-- | Decodes the given VLQ-encoded value into a 32-bit integer.
decode :: ByteString -> Int32
decode = fromVlgSigned . go (0,0) . B.map decodeBase64 where
  go (result,shift) bytes =
    case B.uncons bytes of
      Nothing -> result
      Just (c,next) ->
        let digit = fromIntegral c
            continuation = (digit & continuationBit) /= 0
            digit' = digit & baseMask
            result' = result + (digit' << shift)
            shift' = shift + baseShift
        in if continuation
              then go (result',shift') next
              else result'

decode' :: ByteString -> IO Int32
decode' bs = fromVlgSigned <$> go (0,0) (B.map decodeBase64 bs) where
  go (result,shift) bytes = do
    --putStrLn $ "  go " <> show result <> " " <> show shift
    case B.uncons bytes of
      Nothing -> return result
      Just (c,next) -> do
        --putStrLn $ "  => " <> show c <> " " <> show next
        case result == 0 && c == 0 of
          True -> go (result, shift) next
          False -> do
            let digit = fromIntegral c
            let continuation = (digit & continuationBit) /= 0
            let digit' = digit & baseMask
            let result' = result + (digit' << shift)
            let shift' = shift + baseShift
            case continuation of
              True -> go (result',shift') next
              False -> return result'

-- | Base 64 characters.
base64Chars :: [Word8]
base64Chars = map (fromIntegral.fromEnum) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

-- | Encode the given number to a base 64 character.
encodeBase64 :: Word8 -> Word8
encodeBase64 i = fromMaybe (error "Base 64 char must be between 0 and 63.")
                 (lookup i (zip [0..] base64Chars))

-- | Encode the given base 64 character to a number.
decodeBase64 :: Word8 -> Word8
decodeBase64 i = maybe (error "Not a valid base 65 digit.") toEnum
                 (elemIndex i base64Chars)

-- | Makes the code more familiar to read. Shift-left.
(<<) :: Int32 -> Int -> Int32
(<<) = shiftL

-- | Makes the code more familiar to read. Shift-right.
(>>) :: Int32 -> Int -> Int32
(>>) = shiftR

-- | Makes the code more familiar to read. And.
(&) :: Int32 -> Int32 -> Int32
(&) = (.&.)
