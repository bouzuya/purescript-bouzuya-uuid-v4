module Test.Bouzuya.UUID.V4
  ( tests
  ) where

import Prelude

import Bouzuya.UUID.V4 as UUIDv4
import Data.Array as Array
import Data.Int.Bits as Bits
import Data.Maybe as Maybe
import Data.String.CodeUnits as CodeUnits
import Effect.Class as Class
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Bouzuya.UUID.V4" do
  TestUnit.test "fromArray" do
    let
      a =
        [ 0xd9
        , 0xb0
        , 0x33
        , 0xe7
        , 0x0a
        , 0x44
        , 0x43
        , 0x17
        , 0xbd
        , 0xfc
        , 0x68
        , 0xa2
        , 0x31
        , 0x0d
        , 0x95
        , 0x36
        ]
    Assert.equal
      (Maybe.Just a)
      (map UUIDv4.toArray (UUIDv4.fromArray a))

  TestUnit.test "fromString" do
    Assert.equal
      (Maybe.Just "5626ad70-f184-419b-aa4c-d8c0b8fbe718")
      (map UUIDv4.toString
        (UUIDv4.fromString "5626ad70-f184-419b-aa4c-d8c0b8fbe718"))

  TestUnit.test "generate" do
    uuid <- Class.liftEffect UUIDv4.generate
    Assert.equal 36 (CodeUnits.length (UUIDv4.toString uuid))

  TestUnit.test "toArray" do
    uuid <- Class.liftEffect UUIDv4.generate
    Assert.equal 16 (Array.length (UUIDv4.toArray uuid))
    Assert.equal
      (Maybe.Just 0x40)
      (map (Bits.and 0xf0) (Array.index (UUIDv4.toArray uuid) 6))
    Assert.equal
      (Maybe.Just 0x80)
      (map (Bits.and 0xc0) (Array.index (UUIDv4.toArray uuid) 8))

  TestUnit.test "toString" do
    uuid <- Class.liftEffect UUIDv4.generate
    Assert.equal 36 (CodeUnits.length (UUIDv4.toString uuid))
