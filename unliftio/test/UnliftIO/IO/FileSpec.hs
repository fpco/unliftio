{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnliftIO.IO.FileSpec where

import Test.Hspec
-- Atomic/durable file writing is not supported on Windows.
#ifndef WINDOWS
import Control.Monad (forM_)
import Data.Bool (bool)
import System.FilePath ((</>))
import Test.QuickCheck
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.IO.File as File
import UnliftIO.Temporary (withSystemTempDirectory)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
#if __GLASGOW_HASKELL__ < 820
import Data.Monoid
#endif

data ExpectedException =
  ExpectedException
  deriving (Show)

instance Exception ExpectedException

spec :: Spec
spec = do
  describe "ensureFileDurable" $
    it "ensures a file is durable with an fsync" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> "ensure_file_durable"
        writeFile fp "Hello World"
        File.ensureFileDurable fp
        contents <- B.readFile fp
        contents `shouldBe` "Hello World"
  withBinaryFileSpec False "withBinaryFile" withBinaryFile
  writeBinaryFileSpec "writeBinaryFile" writeBinaryFile
  -- Above two specs are validating the specs behavior by applying to
  -- known good implementations
  withBinaryFileSpec True "withBinaryFileAtomic" File.withBinaryFileAtomic
  writeBinaryFileSpec "writeBinaryFileAtomic" File.writeBinaryFileAtomic
  withBinaryFileSpec False "withBinaryFileDurable" File.withBinaryFileDurable
  writeBinaryFileSpec "writeBinaryFileDurable" File.writeBinaryFileDurable
  withBinaryFileSpec True "withBinaryFileDurableAtomic" File.withBinaryFileDurableAtomic
  writeBinaryFileSpec "writeBinaryFileDurableAtomic" File.writeBinaryFileDurableAtomic

writeFileUtf8 fp str = withBinaryFile fp WriteMode (`BB.hPutBuilder` BB.stringUtf8 str)

withBinaryFileSpec ::
     Bool -- ^ Should we test atomicity
  -> String
  -> (forall a. FilePath -> IOMode -> (Handle -> IO a) -> IO a)
  -> Spec
withBinaryFileSpec atomic fname withFileTestable = do
  let hello = "Hello World"
      helloString = "Hello World"
      writeHello fp = writeFileUtf8 fp helloString
      -- Create a file, write "Hello World" into it and apply the action.
      withHelloFileTestable fp iomode action = do
        writeHello fp
        withFileTestable fp iomode action
      goodbye = "Goodbye yall"
      modifiedPermissions =
        setOwnerExecutable True $
        setOwnerReadable True $ setOwnerWritable True emptyPermissions
  describe fname $ do
    it "read" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-read"
        withHelloFileTestable fp ReadWriteMode (`B.hGet` B.length hello) `shouldReturn`
          hello
    it "write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-write"
        withHelloFileTestable fp WriteMode (`B.hPut` goodbye)
        B.readFile fp `shouldReturn` goodbye
    it "read/write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-read-write"
        withHelloFileTestable fp ReadWriteMode $ \h -> do
          B.hGetLine h `shouldReturn` hello
          B.hPut h goodbye
        B.readFile fp `shouldReturn` (hello <> goodbye)
    it "append" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-append"
            privet = "Привет Мир" -- some unicode won't hurt
            encodeUtf8 = BL.toStrict . BB.toLazyByteString . BB.stringUtf8
        writeFileUtf8 fp privet
        setPermissions fp modifiedPermissions
        withFileTestable fp AppendMode $ \h -> B.hPut h goodbye
        B.readFile fp `shouldReturn` (encodeUtf8 privet <> goodbye)
    it "sub-directory" $
      withSystemTempDirectory "rio" $ \dir -> do
        let subDir = dir </> fname ++ "-sub-directory"
            fp = subDir </> "test.file"
        createDirectoryIfMissing True subDir
        withHelloFileTestable fp ReadWriteMode $ \h -> do
          B.hGetLine h `shouldReturn` hello
          B.hPut h goodbye
        B.readFile fp `shouldReturn` (hello <> goodbye)
    it "relative-directory" $
      withSystemTempDirectory "rio" $ \dir -> do
        let relDir = fname ++ "-relative-directory"
            subDir = dir </> relDir
            fp = relDir </> "test.file"
        createDirectoryIfMissing True subDir
        withCurrentDirectoryCompat dir $ do
          withHelloFileTestable fp ReadWriteMode $ \h -> do
            B.hGetLine h `shouldReturn` hello
            B.hPut h goodbye
          B.readFile fp `shouldReturn` (hello <> goodbye)
    it "modified-permissions" $
      forM_ [WriteMode, ReadWriteMode, AppendMode] $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-modified-permissions"
          writeHello fp
          setPermissions fp modifiedPermissions
          withFileTestable fp iomode $ \h -> B.hPut h goodbye
          getPermissions fp `shouldReturn` modifiedPermissions
    it "exception - Does not corrupt files" $
      bool expectFailure property atomic $ -- should fail for non-atomic
      forM_ [WriteMode, ReadWriteMode, AppendMode] $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-exception"
          _ :: Either ExpectedException () <-
            try $
            withHelloFileTestable fp iomode $ \h -> do
              B.hPut h goodbye
              throwIO ExpectedException
          B.readFile fp `shouldReturn` hello
    it "exception - Does not leave files behind" $
      bool expectFailure property atomic $ -- should fail for non-atomic
      forM_ [WriteMode, ReadWriteMode, AppendMode] $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-exception"
          _ :: Either ExpectedException () <-
            try $
            withFileTestable fp iomode $ \h -> do
              B.hPut h goodbye
              throwIO ExpectedException
          doesFileExist fp `shouldReturn` False
          listDirectoryCompat dir `shouldReturn` []
    it "delete - file" $
      bool expectFailure property atomic $ -- should fail for non-atomic
      forM_ [WriteMode, ReadWriteMode, AppendMode] $ \iomode ->
        withSystemTempDirectory "rio" $ \dir -> do
          let fp = dir </> fname ++ "-delete"
          withHelloFileTestable fp iomode $ \h -> do
            removeFile fp
            B.hPut h goodbye
          doesFileExist fp `shouldReturn` True

writeBinaryFileSpec :: String -> (FilePath -> B.ByteString -> IO ()) -> SpecWith ()
writeBinaryFileSpec fname writeFileTestable = do
  let hello = "Hello World"
  describe fname $ do
    it "write" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-write"
        writeFileTestable fp hello
        B.readFile fp `shouldReturn` hello
    it "default-permissions" $
      withSystemTempDirectory "rio" $ \dir -> do
        let fp = dir </> fname ++ "-default-permissions"
            defaultPermissions =
              setOwnerReadable True $ setOwnerWritable True emptyPermissions
        writeFileTestable fp hello
        getPermissions fp `shouldReturn` defaultPermissions


listDirectoryCompat :: FilePath -> IO [FilePath]
#if MIN_VERSION_directory(1,2,5)
listDirectoryCompat = listDirectory
#else
listDirectoryCompat path =
  filter f <$> getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."
#endif

withCurrentDirectoryCompat :: FilePath -> IO a -> IO a
#if MIN_VERSION_directory(1,2,3)
withCurrentDirectoryCompat = withCurrentDirectory
#else
withCurrentDirectoryCompat dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
#endif

#else
spec :: Spec
spec = pure ()
#endif
