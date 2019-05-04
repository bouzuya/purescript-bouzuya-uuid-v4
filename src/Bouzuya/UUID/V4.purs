module Bouzuya.UUID.V4
  ( UUIDv4
  , fromArray
  , fromString
  , generate
  , toArray
  , toString
  ) where

import Prelude

import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Int as Int
import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable as Traversable
import Effect (Effect)
import Node.Buffer (Buffer, Octet, Offset)
import Node.Buffer as Buffer
import Node.Crypto as Crypto
import Partial.Unsafe as Unsafe
import Unsafe.Coerce as UnsafeCoerce

newtype UUIDv4 = UUIDv4 (Array Octet)

instance showUUIDv4 :: Show UUIDv4 where
  show uuid = "(UUIDv4 " <> toString uuid <> ")"

fromArray :: Array Octet -> Maybe UUIDv4
fromArray os
  | ((Array.length os) /= byteLength) ||
    (Array.any (\o -> o < 0x00 || 0xff < o) os) ||
    ((map (Bits.and 0xf0) (Array.index os 6)) /= Maybe.Just 0x40) ||
    ((map (Bits.and 0xc0) (Array.index os 8)) /= Maybe.Just 0x80)
    = Maybe.Nothing
  | otherwise = Maybe.Just (UUIDv4 os)

-- | RRRRRRRR-RRRR-4RRR-rRRR-RRRRRRRRRRRR
fromString :: String -> Maybe UUIDv4
fromString s
  | (CodeUnits.length s) /= 36 = Maybe.Nothing
  | not
      (Array.all
        (\c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || (c == '-'))
        (CodeUnits.toCharArray s)) = Maybe.Nothing
  | otherwise =
      case String.split (String.Pattern "-") s of
        ss@[s1, s2, s3, s4, s5] -> do
          MonadZero.guard ((CodeUnits.length s1) == 8)
          MonadZero.guard ((CodeUnits.length s2) == 4)
          MonadZero.guard ((CodeUnits.length s3) == 4)
          MonadZero.guard ((CodeUnits.length s4) == 4)
          MonadZero.guard ((CodeUnits.length s5) == 12)
          let
            f :: Array String -> String -> Array String
            f a s'
              | String.null s' = a
              | otherwise =
                  let { before, after } = CodeUnits.splitAt 2 s'
                  in f (Array.snoc a before) after
          map
            UUIDv4
            (Traversable.traverse
              (Int.fromStringAs Int.hexadecimal)
              (f [] (String.joinWith "" ss)))
        _ -> Maybe.Nothing

generate :: Effect UUIDv4
generate = do
  bytes <- (UnsafeCoerce.unsafeCoerce Crypto.randomBytes) byteLength -- FIXME
  _ <- modify ((Bits.or 0x40) <<< (Bits.and 0x0f)) 6 bytes
  _ <- modify ((Bits.or 0x80) <<< (Bits.and 0x3f)) 8 bytes
  map UUIDv4 (Buffer.toArray bytes)

toArray :: UUIDv4 -> Array Octet
toArray (UUIDv4 os) = os

-- | RRRRRRRR-RRRR-4RRR-rRRR-RRRRRRRRRRRR
toString :: UUIDv4 -> String
toString (UUIDv4 os) =
  let
    hex2 i =
      let s = Int.toStringAs Int.hexadecimal i
      in if CodeUnits.length s < 2 then "0" <> s else s
    ss = map hex2 os
  in
    Maybe.maybe'
      (\_ -> Unsafe.unsafeCrashWith "invalid uuid")
      (String.joinWith "")
      (Traversable.sequence
        [ Array.index ss 0
        , Array.index ss 1
        , Array.index ss 2
        , Array.index ss 3
        , Maybe.Just "-"
        , Array.index ss 4
        , Array.index ss 5
        , Maybe.Just "-"
        , Array.index ss 6
        , Array.index ss 7
        , Maybe.Just "-"
        , Array.index ss 8
        , Array.index ss 9
        , Maybe.Just "-"
        , Array.index ss 10
        , Array.index ss 11
        , Array.index ss 12
        , Array.index ss 13
        , Array.index ss 14
        , Array.index ss 15
        ])

-- private

byteLength :: Int
byteLength = 16

modify :: (Octet -> Octet) -> Offset -> Buffer -> Effect Unit
modify f offset buffer = do
  octetMaybe <- Buffer.getAtOffset offset buffer
  case octetMaybe of
    Maybe.Nothing -> pure unit
    Maybe.Just octet -> Buffer.setAtOffset (f octet) offset buffer
