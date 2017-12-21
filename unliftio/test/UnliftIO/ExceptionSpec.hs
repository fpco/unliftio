module UnliftIO.ExceptionSpec (spec) where

import Test.Hspec
import UnliftIO

spec :: Spec
spec = do
  let shouldLeft x = either (const Nothing) Just x `shouldBe` Nothing
      shouldRight x = either (Just . show) (const Nothing) x `shouldBe` Nothing
  describe "pureTry" $ do
    it "Right for defined values" $ shouldRight $ pureTry ()
    it "Left for bottom" $ shouldLeft $ pureTry (undefined :: ())
    it "Right for wrapped bottom" $ shouldRight $ pureTry $ Just (undefined :: ())
  describe "pureTryDeep" $ do
    it "Right for defined values" $ shouldRight $ pureTryDeep ()
    it "Left for bottom" $ shouldLeft $ pureTryDeep (undefined :: ())
    it "Left for wrapped bottom" $ shouldLeft $ pureTryDeep $ Just (undefined :: ())
