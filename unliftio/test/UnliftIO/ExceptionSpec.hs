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

  describe "mapExceptionM" $ do
    it "should convert an exception" $ do
      result <- try $ mapExceptionM (\Exception1 -> Exception2) (throwIO Exception1)
      result `shouldBe` (Left Exception2 :: Either Exception2 ())
    it "should not convert unrelated exceptions" $ do
      result <- try $ mapExceptionM (\Exception1 -> Exception2) (throwIO Exception2)
      result `shouldBe` (Left Exception2 :: Either Exception2 ())

data Exception1 = Exception1 deriving (Show, Eq)
instance Exception Exception1

data Exception2 = Exception2 deriving (Show, Eq)
instance Exception Exception2
