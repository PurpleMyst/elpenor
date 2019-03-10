{-# LANGUAGE OverloadedStrings #-}
module MarshalSpec(spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Lazy.Char8 as B

import AST
import Marshal

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
spec = parallel $ do
  describe "MarshalWithType ByteString" $
    it "should marshal values of type ByteString" $
      marshalRoundTrip ("hello, world" :: ByteString) `shouldReturn` "b'hello, world'"

  describe "MarshalWithType Int32" $
    it "should marshal arbitrary values of type Int32" $
      propertyIO (\n -> (show n ==) <$> marshalRoundTrip (n :: Int32))

  describe "MarshalWithType AST" $ do
    it "should marshal AST values of type Identifier" $
      propertyIO (\s -> (s ==) <$> marshalRoundTrip (Identifier s))

    it "should marshal AST values of type String" $
      propertyIO (\s -> (s ==) <$> marshalRoundTrip (String s))

    it "should marshal AST values of type Number" $
      propertyIO (\n -> (show n ==) <$> marshalRoundTrip (Number n))

  describe "MarshalWithType a => MarshalWithType [a]" $ do
    it "should marshal values of type [Int32]" $
      marshalRoundTrip ([1, 2, 3] :: [Int32]) `shouldReturn` "(1, 2, 3)"

    it "should marshal values of type [AST]" $
      marshalRoundTrip [Number 3, String "foo", Number 5, Identifier "bar"] `shouldReturn` "(3, 'foo', 5, 'bar')"
