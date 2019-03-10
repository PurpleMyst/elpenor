module CompilerSpec(spec) where

import Test.Hspec

spec :: Spec
spec = parallel $
  it "should compile"
    pending
