module UnliftIO.DirectorySpec (spec) where

import Test.Hspec
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
