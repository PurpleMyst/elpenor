{-# LANGUAGE OverloadedStrings #-}
module MarshalSpec(spec, marshalRoundTrip) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import AST
import Marshal

import System.IO
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import System.Process
import System.IO.Temp

marshalRoundTrip :: (MarshalWithType a) => a -> IO String
marshalRoundTrip x = do
  tmpdir <- getCanonicalTemporaryDirectory
  (fname, handle) <- openTempFile tmpdir "marshal.dat"
  B.hPut handle (marshalWithType x)
  hClose handle -- flush
  readProcess "python3" [ "-c", "import marshal; print(marshal.load(open('"  ++ fname ++ "', 'rb')), end='')"] ""

propertyIO :: (Arbitrary a, Show a, Testable b) => (a -> IO b) -> Property
propertyIO = property . fmap (monadicIO . run)

spec :: Spec
spec = parallel $
  describe "Marshal" $ do
    describe "MarshalWithType ByteString" $
      it "should marshal values of type ByteString" $
        marshalRoundTrip ("hello, world" :: ByteString) `shouldReturn` "b'hello, world'"

    describe "MarshalWithType Integer" $
      it "should marshal arbitrary values of type Integer" $
        propertyIO (\n -> (show n ==) <$> marshalRoundTrip (n :: Integer))

    describe "MarshalWithType AST" $ do
      it "should marshal AST values of type Identifier" $
        marshalRoundTrip (Identifier "hello, world") `shouldReturn` "hello, world"

      it "should marshal AST values of type String" $
        propertyIO (\s -> (s ==) <$> marshalRoundTrip (String s))

      it "should marshal AST values of type Number" $
        marshalRoundTrip (Number 42) `shouldReturn` "42"

    describe "MarshalWithType a => MarshalWithType [a]" $ do
      it "should marshal values of type [Integer]" $
        marshalRoundTrip ([1, 2, 3] :: [Integer]) `shouldReturn` "(1, 2, 3)"

      it "should marshal values of type [AST]" $
        marshalRoundTrip [Number 3, String "foo", Number 5, Identifier "bar"] `shouldReturn` "(3, 'foo', 5, 'bar')"
