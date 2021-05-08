{-# LANGUAGE CPP #-}
module UnliftIO.DirectorySpec (spec) where

import Test.Hspec
#if MIN_VERSION_directory(1,3,1)
import System.FilePath
import UnliftIO.IO
import UnliftIO.Directory
import UnliftIO.Temporary


spec :: Spec
spec = do
  describe "createFileLink" $ do
    it "mirror" $ do
      withSystemTempDirectory "createFileLink.mirror"  $ \fp -> do
        let fileContent = "i am the same"
            fileContent' = "I AM THE SAME"
            origin = fp </> "origin.txt"
            link = fp </> "link.txt"
        writeFile origin fileContent
        createFileLink origin link
        linkContent <- readFile link
        linkContent `shouldBe`fileContent
        writeFile origin fileContent'
        linkContent' <- readFile link
        linkContent' `shouldBe`fileContent'
#else
spec :: Spec
spec = pure ()
#endif
