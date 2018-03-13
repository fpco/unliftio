{-# LANGUAGE CPP #-}
-- | Unlifted "Foreign".
--
-- @since 0.2.4.0

module UnliftIO.Foreign (
  -- * Re-exported modules
    module Data.Bits
  , module Data.Int
  , module Data.Word
  , module Foreign.C.Types

  -- * Unlifted "Foreign.C.String"
  , CString
  , CStringLen
  , peekCString
  , peekCStringLen
  , newCString
  , newCStringLen
  , withCString
  , withCStringLen
  , charIsRepresentable
  , castCharToCChar
  , castCCharToChar
  , castCharToCUChar
  , castCUCharToChar
  , castCharToCSChar
  , castCSCharToChar
  , peekCAString
  , peekCAStringLen
  , newCAString
  , newCAStringLen
  , withCAString
  , withCAStringLen
  , CWString
  , CWStringLen
  , peekCWString
  , peekCWStringLen
  , newCWString
  , newCWStringLen
  , withCWString
  , withCWStringLen

  -- * Unlifted "Foreign.C.Error"
  , Errno(..)
  , eOK
  , e2BIG
  , eACCES
  , eADDRINUSE
  , eADDRNOTAVAIL
  , eADV
  , eAFNOSUPPORT
  , eAGAIN
  , eALREADY
  , eBADF
  , eBADMSG
  , eBADRPC
  , eBUSY
  , eCHILD
  , eCOMM
  , eCONNABORTED
  , eCONNREFUSED
  , eCONNRESET
  , eDEADLK
  , eDESTADDRREQ
  , eDIRTY
  , eDOM
  , eDQUOT
  , eEXIST
  , eFAULT
  , eFBIG
  , eFTYPE
  , eHOSTDOWN
  , eHOSTUNREACH
  , eIDRM
  , eILSEQ
  , eINPROGRESS
  , eINTR
  , eINVAL
  , eIO
  , eISCONN
  , eISDIR
  , eLOOP
  , eMFILE
  , eMLINK
  , eMSGSIZE
  , eMULTIHOP
  , eNAMETOOLONG
  , eNETDOWN
  , eNETRESET
  , eNETUNREACH
  , eNFILE
  , eNOBUFS
  , eNODATA
  , eNODEV
  , eNOENT
  , eNOEXEC
  , eNOLCK
  , eNOLINK
  , eNOMEM
  , eNOMSG
  , eNONET
  , eNOPROTOOPT
  , eNOSPC
  , eNOSR
  , eNOSTR
  , eNOSYS
  , eNOTBLK
  , eNOTCONN
  , eNOTDIR
  , eNOTEMPTY
  , eNOTSOCK
  , eNOTSUP
  , eNOTTY
  , eNXIO
  , eOPNOTSUPP
  , ePERM
  , ePFNOSUPPORT
  , ePIPE
  , ePROCLIM
  , ePROCUNAVAIL
  , ePROGMISMATCH
  , ePROGUNAVAIL
  , ePROTO
  , ePROTONOSUPPORT
  , ePROTOTYPE
  , eRANGE
  , eREMCHG
  , eREMOTE
  , eROFS
  , eRPCMISMATCH
  , eRREMOTE
  , eSHUTDOWN
  , eSOCKTNOSUPPORT
  , eSPIPE
  , eSRCH
  , eSRMNT
  , eSTALE
  , eTIME
  , eTIMEDOUT
  , eTOOMANYREFS
  , eTXTBSY
  , eUSERS
  , eWOULDBLOCK
  , eXDEV
  , isValidErrno
  , getErrno
  , resetErrno
  , errnoToIOError
  , throwErrno
  , throwErrnoIf
  , throwErrnoIf_
  , throwErrnoIfRetry
  , throwErrnoIfRetry_
  , throwErrnoIfMinus1
  , throwErrnoIfMinus1_
  , throwErrnoIfMinus1Retry
  , throwErrnoIfMinus1Retry_
  , throwErrnoIfNull
  , throwErrnoIfNullRetry
  , throwErrnoIfRetryMayBlock
  , throwErrnoIfRetryMayBlock_
  , throwErrnoIfMinus1RetryMayBlock
  , throwErrnoIfMinus1RetryMayBlock_
  , throwErrnoIfNullRetryMayBlock
  , throwErrnoPath
  , throwErrnoPathIf
  , throwErrnoPathIf_
  , throwErrnoPathIfNull
  , throwErrnoPathIfMinus1
  , throwErrnoPathIfMinus1_

  -- * Unlifted "Foreign.Ptr"
  , Ptr
  , nullPtr
  , castPtr
  , plusPtr
  , alignPtr
  , minusPtr
  , FunPtr
  , nullFunPtr
  , castFunPtr
  , castFunPtrToPtr
  , castPtrToFunPtr
  , freeHaskellFunPtr
  , IntPtr(..)
  , ptrToIntPtr
  , intPtrToPtr
  , WordPtr(..)
  , ptrToWordPtr
  , wordPtrToPtr

  -- * Unlifted "Foreign.ForeignPtr"
  , ForeignPtr
  , FinalizerPtr
  , FinalizerEnvPtr
  , newForeignPtr
  , newForeignPtr_
  , addForeignPtrFinalizer
  , newForeignPtrEnv
  , addForeignPtrFinalizerEnv
  , withForeignPtr
  , finalizeForeignPtr
  , touchForeignPtr
  , castForeignPtr
#if MIN_VERSION_base(4,10,0)
  , plusForeignPtr
#endif
  , mallocForeignPtr
  , mallocForeignPtrBytes
  , mallocForeignPtrArray
  , mallocForeignPtrArray0
  , newGHCForeignPtr
  , addGHCForeignPtrFinalizer
  , unsafeForeignPtrToPtr

  -- * Unlifted "Foreign.StablePtr"
  , StablePtr
  , newStablePtr
  , deRefStablePtr
  , freeStablePtr
  , castStablePtrToPtr
  , castPtrToStablePtr

  -- * Unlifted "Foreign.Storable"
  , Storable(..)

  -- * Unlifted "Foreign.Marshal.Alloc"
  , alloca
  , allocaBytes
  , allocaBytesAligned
  , malloc
  , mallocBytes
#if MIN_VERSION_base(4,8,0)
  , calloc
  , callocBytes
#endif
  , realloc
  , reallocBytes
  , free
  , finalizerFree

  -- * Unlifted "Foreign.Marshal.Array"
  , mallocArray
  , mallocArray0
  , allocaArray
  , allocaArray0
  , reallocArray
  , reallocArray0
#if MIN_VERSION_base(4,8,0)
  , callocArray
  , callocArray0
#endif
  , peekArray
  , peekArray0
  , pokeArray
  , pokeArray0
  , newArray
  , newArray0
  , withArray
  , withArray0
  , withArrayLen
  , withArrayLen0
  , copyArray
  , moveArray
  , lengthArray0
  , advancePtr

  -- * Unlifted "Foreign.Marshal.Error"
  , throwIf
  , throwIf_
  , throwIfNeg
  , throwIfNeg_
  , throwIfNull

  -- * Unlifted "Foreign.Marshal.Pool"
  , Pool
  , newPool
  , freePool
  , withPool
  , pooledMalloc
  , pooledMallocBytes
  , pooledRealloc
  , pooledReallocBytes
  , pooledMallocArray
  , pooledMallocArray0
  , pooledReallocArray
  , pooledReallocArray0
  , pooledNew
  , pooledNewArray
  , pooledNewArray0

  -- * Unlifted "Foreign.Marshal.Utils"
  , with
  , new
  , fromBool
  , toBool
  , maybeNew
  , maybeWith
  , maybePeek
  , withMany
  , copyBytes
  , moveBytes
#if MIN_VERSION_base(4,8,0)
  , fillBytes
#endif
  ) where

import Control.Monad.IO.Unlift
import Data.Bits
import Data.Int
import Data.Word
import qualified Foreign as F
import Foreign
  ( FinalizerEnvPtr
  , FinalizerPtr
  , ForeignPtr
  , FunPtr
  , IntPtr(..)
  , Pool
  , Ptr
  , StablePtr
  , Storable(..)
  , WordPtr(..)
  , advancePtr
  , alignPtr
  , castForeignPtr
  , castFunPtr
  , castFunPtrToPtr
  , castPtr
  , castPtrToFunPtr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , finalizerFree
  , fromBool
  , intPtrToPtr
  , minusPtr
  , nullFunPtr
  , nullPtr
#if MIN_VERSION_base(4,10,0)
  , plusForeignPtr
#endif
  , plusPtr
  , ptrToIntPtr
  , ptrToWordPtr
  , toBool
  , withMany
  , wordPtrToPtr
  )

import qualified Foreign.C as F
import Foreign.C
  ( CString
  , CStringLen
  , CWString
  , CWStringLen
  , Errno(..)
  , castCCharToChar
  , castCSCharToChar
  , castCUCharToChar
  , castCharToCChar
  , castCharToCSChar
  , castCharToCSChar
  , castCharToCUChar
  , charIsRepresentable
  , e2BIG
  , eACCES
  , eADDRINUSE
  , eADDRNOTAVAIL
  , eADV
  , eAFNOSUPPORT
  , eAGAIN
  , eALREADY
  , eBADF
  , eBADMSG
  , eBADRPC
  , eBUSY
  , eCHILD
  , eCOMM
  , eCONNABORTED
  , eCONNREFUSED
  , eCONNRESET
  , eDEADLK
  , eDESTADDRREQ
  , eDIRTY
  , eDOM
  , eDQUOT
  , eEXIST
  , eFAULT
  , eFBIG
  , eFTYPE
  , eHOSTDOWN
  , eHOSTUNREACH
  , eIDRM
  , eILSEQ
  , eINPROGRESS
  , eINTR
  , eINVAL
  , eIO
  , eISCONN
  , eISDIR
  , eLOOP
  , eMFILE
  , eMLINK
  , eMSGSIZE
  , eMULTIHOP
  , eNAMETOOLONG
  , eNETDOWN
  , eNETRESET
  , eNETUNREACH
  , eNFILE
  , eNOBUFS
  , eNODATA
  , eNODEV
  , eNOENT
  , eNOEXEC
  , eNOLCK
  , eNOLINK
  , eNOMEM
  , eNOMSG
  , eNONET
  , eNOPROTOOPT
  , eNOSPC
  , eNOSR
  , eNOSTR
  , eNOSYS
  , eNOTBLK
  , eNOTCONN
  , eNOTDIR
  , eNOTEMPTY
  , eNOTSOCK
  , eNOTSUP
  , eNOTTY
  , eNXIO
  , eOK
  , eOPNOTSUPP
  , ePERM
  , ePFNOSUPPORT
  , ePIPE
  , ePROCLIM
  , ePROCUNAVAIL
  , ePROGMISMATCH
  , ePROGUNAVAIL
  , ePROTO
  , ePROTONOSUPPORT
  , ePROTOTYPE
  , eRANGE
  , eREMCHG
  , eREMOTE
  , eROFS
  , eRPCMISMATCH
  , eRREMOTE
  , eSHUTDOWN
  , eSOCKTNOSUPPORT
  , eSPIPE
  , eSRCH
  , eSRMNT
  , eSTALE
  , eTIME
  , eTIMEDOUT
  , eTOOMANYREFS
  , eTXTBSY
  , eUSERS
  , eWOULDBLOCK
  , eXDEV
  , errnoToIOError
  , isValidErrno
  )
import Foreign.C.Types
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr.Unsafe

-- | Lifted 'F.peekCString'.
--
-- @since 0.2.4.0
{-# INLINE peekCString #-}
peekCString :: MonadIO m => CString -> m String
peekCString = liftIO . F.peekCString

-- | Lifted 'F.peekCStringLen'.
--
-- @since 0.2.4.0
{-# INLINE peekCStringLen #-}
peekCStringLen :: MonadIO m => CStringLen -> m String
peekCStringLen = liftIO . F.peekCStringLen

-- | Lifted 'F.newCString'.
--
-- @since 0.2.4.0
{-# INLINE newCString #-}
newCString :: MonadIO m => String -> m CString
newCString = liftIO . F.newCString

-- | Lifted 'F.newCStringLen'.
--
-- @since 0.2.4.0
{-# INLINE newCStringLen #-}
newCStringLen :: MonadIO m => String -> m CStringLen
newCStringLen = liftIO . F.newCStringLen

-- | Unlifted 'F.withCString'.
--
-- @since 0.2.4.0
{-# INLINE withCString #-}
withCString :: MonadUnliftIO m => String -> (CString -> m a) -> m a
withCString s f = withRunInIO (\u -> F.withCString s (u . f))

-- | Unlifted 'F.withCStringLen'.
--
-- @since 0.2.4.0
{-# INLINE withCStringLen #-}
withCStringLen :: MonadUnliftIO m => String -> (CStringLen -> m a) -> m a
withCStringLen s f = withRunInIO (\u -> F.withCStringLen s (u . f))

-- | Lifted 'F.peekCAString'.
--
-- @since 0.2.4.0
{-# INLINE peekCAString #-}
peekCAString :: MonadIO m => CString -> m String
peekCAString = liftIO . F.peekCAString

-- | Lifted 'F.peekCAStringLen'.
--
-- @since 0.2.4.0
{-# INLINE peekCAStringLen #-}
peekCAStringLen :: MonadIO m => CStringLen -> m String
peekCAStringLen = liftIO . F.peekCAStringLen

-- | Lifted 'F.newCAString'.
--
-- @since 0.2.4.0
{-# INLINE newCAString #-}
newCAString :: MonadIO m => String -> m CString
newCAString = liftIO . F.newCAString

-- | Lifted 'F.newCAStringLen'.
--
-- @since 0.2.4.0
{-# INLINE newCAStringLen #-}
newCAStringLen :: MonadIO m => String -> m CStringLen
newCAStringLen = liftIO . F.newCAStringLen

-- | Unlifted 'F.withCAString'.
--
-- @since 0.2.4.0
{-# INLINE withCAString #-}
withCAString :: MonadUnliftIO m => String -> (CString -> m a) -> m a
withCAString str f = withRunInIO (\u -> F.withCAString str (u . f))

-- | Unlifted 'F.withCAStringLen'.
--
-- @since 0.2.4.0
{-# INLINE withCAStringLen #-}
withCAStringLen :: MonadUnliftIO m => String -> (CStringLen -> m a) -> m a
withCAStringLen str f = withRunInIO (\u -> F.withCAStringLen str (u . f))

-- | Lifted 'F.peekCWString'.
--
-- @since 0.2.4.0
{-# INLINE peekCWString #-}
peekCWString :: MonadIO m => CWString -> m String
peekCWString = liftIO . F.peekCWString

-- | Lifted 'F.peekCWStringLen'.
--
-- @since 0.2.4.0
{-# INLINE peekCWStringLen #-}
peekCWStringLen :: MonadIO m => CWStringLen -> m String
peekCWStringLen = liftIO . F.peekCWStringLen

-- | Lifted 'F.newCWString'.
--
-- @since 0.2.4.0
{-# INLINE newCWString #-}
newCWString :: MonadIO m => String -> m CWString
newCWString = liftIO . F.newCWString

-- | Lifted 'F.newCWStringLen'.
--
-- @since 0.2.4.0
{-# INLINE newCWStringLen #-}
newCWStringLen :: MonadIO m => String -> m CWStringLen
newCWStringLen = liftIO . F.newCWStringLen

-- | Unlifted 'F.withCWString'.
--
-- @since 0.2.4.0
{-# INLINE withCWString #-}
withCWString :: MonadUnliftIO m => String -> (CWString -> m a) -> m a
withCWString str f = withRunInIO (\u -> F.withCWString str (u . f))

-- | Unlifted 'F.withCWStringLen'.
--
-- @since 0.2.4.0
{-# INLINE withCWStringLen #-}
withCWStringLen :: MonadUnliftIO m => String -> (CWStringLen -> m a) -> m a
withCWStringLen str f = withRunInIO (\u -> F.withCWStringLen str (u . f))

-- | Lifted 'F.getErrno'.
--
-- @since 0.2.4.0
{-# INLINE getErrno #-}
getErrno :: MonadIO m => m Errno
getErrno = liftIO F.getErrno

-- | Lifted 'F.resetErrno'.
--
-- @since 0.2.4.0
{-# INLINE resetErrno #-}
resetErrno :: MonadIO m => m ()
resetErrno = liftIO F.resetErrno

-- | Lifted 'F.throwErrno'.
--
-- @since 0.2.4.0
{-# INLINE throwErrno #-}
throwErrno :: MonadIO m => String -> m a
throwErrno = liftIO . F.throwErrno

-- | Unlifted 'F.throwErrnoIf'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIf #-}
throwErrnoIf :: MonadUnliftIO m => (a -> Bool) -> String -> m a -> m a
throwErrnoIf pred_ loc f = withRunInIO (\u -> F.throwErrnoIf pred_ loc (u f))

-- | Unlifted 'F.throwErrnoIf_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIf_ #-}
throwErrnoIf_ :: MonadUnliftIO m => (a -> Bool) -> String -> m a -> m ()
throwErrnoIf_ pred_ loc f = withRunInIO (\u -> F.throwErrnoIf_ pred_ loc (u f))

-- | Unlifted 'F.throwErrnoIfRetry'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfRetry #-}
throwErrnoIfRetry :: MonadUnliftIO m => (a -> Bool) -> String -> m a -> m a
throwErrnoIfRetry pred_ loc f =
  withRunInIO (\u -> F.throwErrnoIfRetry pred_ loc (u f))

-- | Unlifted 'F.throwErrnoIfRetry_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfRetry_ #-}
throwErrnoIfRetry_ :: MonadUnliftIO m => (a -> Bool) -> String -> m a -> m ()
throwErrnoIfRetry_ pred_ loc f =
  withRunInIO (\u -> F.throwErrnoIfRetry_ pred_ loc (u f))

-- | Unlifted 'F.throwErrnoIfMinus1'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1 #-}
throwErrnoIfMinus1 :: (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m a
throwErrnoIfMinus1 loc f = withRunInIO (\u -> F.throwErrnoIfMinus1 loc (u f))

-- | Unlifted 'F.throwErrnoIfMinus1_'
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1_ #-}
throwErrnoIfMinus1_ :: (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m ()
throwErrnoIfMinus1_ loc f = withRunInIO (\u -> F.throwErrnoIfMinus1_ loc (u f))

-- | Unlifted 'F.throwErrnoIfMinus1Retry'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1Retry #-}
throwErrnoIfMinus1Retry ::
     (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m a
throwErrnoIfMinus1Retry loc f =
  withRunInIO (\u -> F.throwErrnoIfMinus1Retry loc (u f))

-- | Unlifted 'F.throwErrnoIfMinus1Retry_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1Retry_ #-}
throwErrnoIfMinus1Retry_ ::
     (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m ()
throwErrnoIfMinus1Retry_ loc f =
  withRunInIO (\u -> F.throwErrnoIfMinus1Retry_ loc (u f))

-- | Unlifted 'F.throwErrnoIfNull'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfNull #-}
throwErrnoIfNull :: MonadUnliftIO m => String -> m (Ptr a) -> m (Ptr a)
throwErrnoIfNull loc f = withRunInIO (\u -> F.throwErrnoIfNull loc (u f))

-- | Unlifted 'F.throwErrnoIfNullRetry'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfNullRetry #-}
throwErrnoIfNullRetry :: MonadUnliftIO m => String -> m (Ptr a) -> m (Ptr a)
throwErrnoIfNullRetry loc f =
  withRunInIO (\u -> F.throwErrnoIfNullRetry loc (u f))

-- | Unlifted 'F.throwErrnoIfRetryMayBlock'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfRetryMayBlock #-}
throwErrnoIfRetryMayBlock ::
     MonadUnliftIO m => (a -> Bool) -> String -> m a -> m b -> m a
throwErrnoIfRetryMayBlock pred_ loc f on_block =
  withRunInIO (\u -> F.throwErrnoIfRetryMayBlock pred_ loc (u f) (u on_block))

-- | Unlifted 'F.throwErrnoIfRetryMayBlock_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfRetryMayBlock_ #-}
throwErrnoIfRetryMayBlock_ ::
     MonadUnliftIO m => (a -> Bool) -> String -> m a -> m b -> m ()
throwErrnoIfRetryMayBlock_ pred_ loc f on_block =
  withRunInIO (\u -> F.throwErrnoIfRetryMayBlock_ pred_ loc (u f) (u on_block))

-- | Unlifted 'F.throwErrnoIfMinus1RetryMayBlock'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1RetryMayBlock #-}
throwErrnoIfMinus1RetryMayBlock ::
     (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m b -> m a
throwErrnoIfMinus1RetryMayBlock loc f on_block =
  withRunInIO (\u -> F.throwErrnoIfMinus1RetryMayBlock loc (u f) (u on_block))

-- | Unlifted 'F.throwErrnoIfMinus1RetryMayBlock_'
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfMinus1RetryMayBlock_ #-}
throwErrnoIfMinus1RetryMayBlock_ ::
     (MonadUnliftIO m, Eq a, Num a) => String -> m a -> m b -> m ()
throwErrnoIfMinus1RetryMayBlock_ loc f on_block =
  withRunInIO (\u -> F.throwErrnoIfMinus1RetryMayBlock_ loc (u f) (u on_block))

-- | Unlifted 'F.throwErrnoIfNullRetryMayBlock'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoIfNullRetryMayBlock #-}
throwErrnoIfNullRetryMayBlock ::
     MonadUnliftIO m => String -> m (Ptr a) -> m b -> m (Ptr a)
throwErrnoIfNullRetryMayBlock loc f on_block =
  withRunInIO (\u -> F.throwErrnoIfNullRetryMayBlock loc (u f) (u on_block))

-- | Lifted 'F.throwErrnoPath'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPath #-}
throwErrnoPath :: MonadIO m => String -> FilePath -> m a
throwErrnoPath loc path = liftIO (F.throwErrnoPath loc path)

-- | Unlifted 'F.throwErrnoPathIf'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPathIf #-}
throwErrnoPathIf ::
     MonadUnliftIO m => (a -> Bool) -> String -> FilePath -> m a -> m a
throwErrnoPathIf pred_ loc path f =
  withRunInIO (\u -> F.throwErrnoPathIf pred_ loc path (u f))

-- | Unlifted 'F.throwErrnoPathIf_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPathIf_ #-}
throwErrnoPathIf_ ::
     MonadUnliftIO m => (a -> Bool) -> String -> FilePath -> m a -> m ()
throwErrnoPathIf_ pred_ loc path f =
  withRunInIO (\u -> F.throwErrnoPathIf_ pred_ loc path (u f))

-- | Unlifted 'F.throwErrnoPathIfNull'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPathIfNull #-}
throwErrnoPathIfNull ::
     MonadUnliftIO m => String -> FilePath -> m (Ptr a) -> m (Ptr a)
throwErrnoPathIfNull loc path f =
  withRunInIO (\u -> F.throwErrnoPathIfNull loc path (u f))

-- | Unlifted 'F.throwErrnoPathIfMinus1'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPathIfMinus1 #-}
throwErrnoPathIfMinus1 ::
     (MonadUnliftIO m, Eq a, Num a) => String -> FilePath -> m a -> m a
throwErrnoPathIfMinus1 loc path f =
  withRunInIO (\u -> F.throwErrnoPathIfMinus1 loc path (u f))

-- | Unlifted 'F.throwErrnoPathIfMinus1_'.
--
-- @since 0.2.4.0
{-# INLINE throwErrnoPathIfMinus1_ #-}
throwErrnoPathIfMinus1_ ::
     (MonadUnliftIO m, Eq a, Num a) => String -> FilePath -> m a -> m ()
throwErrnoPathIfMinus1_ loc path f =
  withRunInIO (\u -> F.throwErrnoPathIfMinus1_ loc path (u f))

-- | Lifted 'F.freeHaskellFunPtr'.
--
-- @since 0.2.4.0
{-# INLINE freeHaskellFunPtr #-}
freeHaskellFunPtr :: MonadIO m => FunPtr a -> m ()
freeHaskellFunPtr = liftIO . F.freeHaskellFunPtr

-- | Lifted 'F.newForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE newForeignPtr #-}
newForeignPtr :: MonadIO m => FinalizerPtr a -> Ptr a -> m (ForeignPtr a)
newForeignPtr finalizer p = liftIO (F.newForeignPtr finalizer p)

-- | Lifted 'F.newForeignPtr_'.
--
-- @since 0.2.4.0
{-# INLINE newForeignPtr_ #-}
newForeignPtr_ :: MonadIO m => Ptr a -> m (ForeignPtr a)
newForeignPtr_ = liftIO . F.newForeignPtr_

-- | Lifted 'F.addForeignPtrFinalizer'.
--
-- @since 0.2.4.0
{-# INLINE addForeignPtrFinalizer #-}
addForeignPtrFinalizer :: MonadIO m => FinalizerPtr a -> ForeignPtr a -> m ()
addForeignPtrFinalizer finalizer_ptr foreign_ptr =
  liftIO (F.addForeignPtrFinalizer finalizer_ptr foreign_ptr)

-- | Lifted 'F.newForeignPtrEnv'.
--
-- @since 0.2.4.0
{-# INLINE newForeignPtrEnv #-}
newForeignPtrEnv ::
     MonadIO m => FinalizerEnvPtr env a -> Ptr env -> Ptr a -> m (ForeignPtr a)
newForeignPtrEnv finalizer env p = liftIO (F.newForeignPtrEnv finalizer env p)

-- | Lifted 'F.addForeignPtrFinalizerEnv'.
--
-- @since 0.2.4.0
{-# INLINE addForeignPtrFinalizerEnv #-}
addForeignPtrFinalizerEnv ::
     MonadIO m => FinalizerEnvPtr env a -> Ptr env -> ForeignPtr a -> m ()
addForeignPtrFinalizerEnv finalizer env fp =
  liftIO (F.addForeignPtrFinalizerEnv finalizer env fp)

-- | Unlifted 'F.withForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE withForeignPtr #-}
withForeignPtr :: MonadUnliftIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr fo io = withRunInIO (\u -> F.withForeignPtr fo (u . io))

-- | Lifted 'F.finalizeForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE finalizeForeignPtr #-}
finalizeForeignPtr :: MonadIO m => ForeignPtr a -> m ()
finalizeForeignPtr = liftIO . F.finalizeForeignPtr

-- | Lifted 'F.touchForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE touchForeignPtr #-}
touchForeignPtr :: MonadIO m => ForeignPtr a -> m ()
touchForeignPtr = liftIO . F.touchForeignPtr

-- | Lifted 'F.mallocForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE mallocForeignPtr #-}
mallocForeignPtr :: (MonadIO m, Storable a) => m (ForeignPtr a)
mallocForeignPtr = liftIO F.mallocForeignPtr

-- | Lifted 'F.mallocForeignPtrBytes'.
--
-- @since 0.2.4.0
{-# INLINE mallocForeignPtrBytes #-}
mallocForeignPtrBytes :: MonadIO m => Int -> m (ForeignPtr a)
mallocForeignPtrBytes = liftIO . F.mallocForeignPtrBytes

-- | Lifted 'F.mallocForeignPtrArray'.
--
-- @since 0.2.4.0
{-# INLINE mallocForeignPtrArray #-}
mallocForeignPtrArray :: (MonadIO m, Storable a) => Int -> m (ForeignPtr a)
mallocForeignPtrArray = liftIO . F.mallocForeignPtrArray

-- | Lifted 'F.mallocForeignPtrArray0'.
--
-- @since 0.2.4.0
{-# INLINE mallocForeignPtrArray0 #-}
mallocForeignPtrArray0 :: (MonadIO m, Storable a) => Int -> m (ForeignPtr a)
mallocForeignPtrArray0 = liftIO . F.mallocForeignPtrArray0

-- | Unlifted 'FC.newForeignPtr'.
--
-- @since 0.2.4.0
{-# INLINE newGHCForeignPtr #-}
newGHCForeignPtr :: MonadUnliftIO m => Ptr a -> m () -> m (ForeignPtr a)
newGHCForeignPtr ptr f = withRunInIO (\u -> FC.newForeignPtr ptr (u f))

-- | Unlifted 'FC.addForeignPtrFinalizer'.
--
-- @since 0.2.4.0
{-# INLINE addGHCForeignPtrFinalizer #-}
addGHCForeignPtrFinalizer :: MonadUnliftIO m => ForeignPtr a -> m () -> m ()
addGHCForeignPtrFinalizer fptr f =
  withRunInIO (\u -> FC.addForeignPtrFinalizer fptr (u f))

-- | Lifted 'F.newStablePtr'.
--
-- @since 0.2.4.0
{-# INLINE newStablePtr #-}
newStablePtr :: MonadIO m => a -> m (StablePtr a)
newStablePtr = liftIO . F.newStablePtr

-- | Lifted 'F.deRefStablePtr'.
--
-- @since 0.2.4.0
{-# INLINE deRefStablePtr #-}
deRefStablePtr :: MonadIO m => StablePtr a -> m a
deRefStablePtr = liftIO . F.deRefStablePtr

-- | Lifted 'F.freeStablePtr'.
--
-- @since 0.2.4.0
{-# INLINE freeStablePtr #-}
freeStablePtr :: MonadIO m => StablePtr a -> m ()
freeStablePtr = liftIO . F.freeStablePtr

-- | Unlifted 'F.alloca'.
--
-- @since 0.2.4.0
{-# INLINE alloca #-}
alloca :: (MonadUnliftIO m, Storable a) => (Ptr a -> m b) -> m b
alloca action = withRunInIO (\u -> F.alloca (u . action))

-- | Unlifted 'F.allocaBytes'.
--
-- @since 0.2.4.0
{-# INLINE allocaBytes #-}
allocaBytes :: MonadUnliftIO m => Int -> (Ptr a -> m b) -> m b
allocaBytes size action = withRunInIO (\u -> F.allocaBytes size (u . action))

-- | Unlifted 'F.allocaBytesAligned'.
--
-- @since 0.2.4.0
{-# INLINE allocaBytesAligned #-}
allocaBytesAligned :: MonadUnliftIO m => Int -> Int -> (Ptr a -> m b) -> m b
allocaBytesAligned size align action =
  withRunInIO (\u -> F.allocaBytesAligned size align (u . action))

-- | Lifted 'F.malloc'.
--
-- @since 0.2.4.0
{-# INLINE malloc #-}
malloc :: (MonadIO m, Storable a) => m (Ptr a)
malloc = liftIO F.malloc

-- | Lifted 'F.mallocBytes'.
--
-- @since 0.2.4.0
{-# INLINE mallocBytes #-}
mallocBytes :: MonadIO m => Int -> m (Ptr a)
mallocBytes = liftIO . F.mallocBytes

#if MIN_VERSION_base(4,8,0)
-- | Lifted 'F.calloc'.
--
-- @since 0.2.4.0
{-# INLINE calloc #-}
calloc :: (MonadIO m, Storable a) => m (Ptr a)
calloc = liftIO F.calloc

-- | Lifted 'F.callocBytes'.
--
-- @since 0.2.4.0
{-# INLINE callocBytes #-}
callocBytes :: MonadIO m => Int -> m (Ptr a)
callocBytes = liftIO . F.callocBytes
#endif

-- | Lifted 'F.realloc'.
--
-- @since 0.2.4.0
{-# INLINE realloc #-}
realloc :: (MonadIO m, Storable b) => Ptr a -> m (Ptr b)
realloc = liftIO . F.realloc

-- | Lifted 'F.reallocBytes'.
--
-- @since 0.2.4.0
{-# INLINE reallocBytes #-}
reallocBytes :: MonadIO m => Ptr a -> Int -> m (Ptr a)
reallocBytes ptr size = liftIO (F.reallocBytes ptr size)

-- | Lifted 'F.free'.
--
-- @since 0.2.4.0
{-# INLINE free #-}
free :: MonadIO m => Ptr a -> m ()
free = liftIO . F.free

-- | Lifted 'F.mallocArray'.
--
-- @since 0.2.4.0
{-# INLINE mallocArray #-}
mallocArray :: (MonadIO m, Storable a) => Int -> m (Ptr a)
mallocArray = liftIO . F.mallocArray

-- | Lifted 'F.mallocArray0'.
--
-- @since 0.2.4.0
{-# INLINE mallocArray0 #-}
mallocArray0 :: (MonadIO m, Storable a) => Int -> m (Ptr a)
mallocArray0 = liftIO . F.mallocArray

-- | Unlifted 'F.allocaArray'.
--
-- @since 0.2.4.0
{-# INLINE allocaArray #-}
allocaArray :: (MonadUnliftIO m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray size action = withRunInIO (\u -> F.allocaArray size (u . action))

-- | Unlifted 'F.allocaArray0'.
--
-- @since 0.2.4.0
{-# INLINE allocaArray0 #-}
allocaArray0 :: (MonadUnliftIO m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray0 size action = withRunInIO (\u -> F.allocaArray0 size (u . action))

-- | Lifted 'F.reallocArray'.
--
-- @since 0.2.4.0
{-# INLINE reallocArray #-}
reallocArray :: (MonadIO m, Storable a) => Ptr a -> Int -> m (Ptr a)
reallocArray ptr size = liftIO (F.reallocArray ptr size)

-- | Lifted 'F.reallocArray0'.
--
-- @since 0.2.4.0
{-# INLINE reallocArray0 #-}
reallocArray0 :: (MonadIO m, Storable a) => Ptr a -> Int -> m (Ptr a)
reallocArray0 ptr size = liftIO (F.reallocArray0 ptr size)

#if MIN_VERSION_base(4,8,0)
-- | Lifted 'F.callocArray'.
--
-- @since 0.2.4.0
{-# INLINE callocArray #-}
callocArray :: (MonadIO m, Storable a) => Int -> m (Ptr a)
callocArray = liftIO . F.callocArray

-- | Lifted 'F.callocArray0'.
--
-- @since 0.2.4.0
{-# INLINE callocArray0 #-}
callocArray0 :: (MonadIO m, Storable a) => Int -> m (Ptr a)
callocArray0 = liftIO . F.callocArray0
#endif

-- | Lifted 'F.peekArray'.
--
-- @since 0.2.4.0
{-# INLINE peekArray #-}
peekArray :: (MonadIO m, Storable a) => Int -> Ptr a -> m [a]
peekArray size ptr = liftIO (F.peekArray size ptr)

-- | Lifted 'F.peekArray0'.
--
-- @since 0.2.4.0
{-# INLINE peekArray0 #-}
peekArray0 :: (MonadIO m, Storable a, Eq a) => a -> Ptr a -> m [a]
peekArray0 marker ptr = liftIO (F.peekArray0 marker ptr)

-- | Lifted 'F.pokeArray'.
--
-- @since 0.2.4.0
{-# INLINE pokeArray #-}
pokeArray :: (MonadIO m, Storable a) => Ptr a -> [a] -> m ()
pokeArray ptr vals0 = liftIO (F.pokeArray ptr vals0)

-- | Lifted 'F.pokeArray0'.
--
-- @since 0.2.4.0
{-# INLINE pokeArray0 #-}
pokeArray0 :: (MonadIO m, Storable a) => a -> Ptr a -> [a] -> m ()
pokeArray0 marker ptr vals0 = liftIO (F.pokeArray0 marker ptr vals0)

-- | Lifted 'F.newArray'.
--
-- @since 0.2.4.0
{-# INLINE newArray #-}
newArray :: (MonadIO m, Storable a) => [a] -> m (Ptr a)
newArray = liftIO . F.newArray

-- | Lifted 'F.newArray0'
--
-- @since 0.2.4.0
{-# INLINE newArray0 #-}
newArray0 :: (MonadIO m, Storable a) => a -> [a] -> m (Ptr a)
newArray0 marker vals = liftIO (F.newArray0 marker vals)

-- | Unlifted 'F.withArray'.
--
-- @since 0.2.4.0
{-# INLINE withArray #-}
withArray :: (MonadUnliftIO m, Storable a) => [a] -> (Ptr a -> m b) -> m b
withArray vals action = withRunInIO (\u -> F.withArray vals (u . action))

-- | Unlifted 'F.withArray0'.
--
-- @since 0.2.4.0
{-# INLINE withArray0 #-}
withArray0 :: (MonadUnliftIO m, Storable a) => a -> [a] -> (Ptr a -> m b) -> m b
withArray0 marker vals action =
  withRunInIO (\u -> F.withArray0 marker vals (u . action))

-- | Unlifted 'F.withArrayLen'.
--
-- @since 0.2.4.0
{-# INLINE withArrayLen #-}
withArrayLen ::
     (MonadUnliftIO m, Storable a) => [a] -> (Int -> Ptr a -> m b) -> m b
withArrayLen vals f =
  withRunInIO (\u -> F.withArrayLen vals (\s p -> u (f s p)))

-- | Unlifted 'F.withArrayLen0'.
--
-- @since 0.2.4.0
{-# INLINE withArrayLen0 #-}
withArrayLen0 ::
     (MonadUnliftIO m, Storable a) => a -> [a] -> (Int -> Ptr a -> m b) -> m b
withArrayLen0 marker vals f =
  withRunInIO (\u -> F.withArrayLen0 marker vals (\s p -> u (f s p)))

-- | Lifted 'F.copyArray'.
--
-- @since 0.2.4.0
{-# INLINE copyArray #-}
copyArray :: (MonadIO m, Storable a) => Ptr a -> Ptr a -> Int -> m ()
copyArray dest src size = liftIO (F.copyArray dest src size)

-- | Lifted 'F.moveArray'.
--
-- @since 0.2.4.0
{-# INLINE moveArray #-}
moveArray :: (MonadIO m, Storable a) => Ptr a -> Ptr a -> Int -> m ()
moveArray dest src size = liftIO (F.moveArray dest src size)

-- | Lifted 'F.lengthArray0'.
--
-- @since 0.2.4.0
{-# INLINE lengthArray0 #-}
lengthArray0 :: (MonadIO m, Storable a, Eq a) => a -> Ptr a -> m Int
lengthArray0 marker ptr = liftIO (F.lengthArray0 marker ptr)

-- | Unlifted 'F.throwIf'.
--
-- @since 0.2.4.0
{-# INLINE throwIf #-}
throwIf :: MonadUnliftIO m => (a -> Bool) -> (a -> String) -> m a -> m a
throwIf pred_ msgfct act = withRunInIO (\u -> F.throwIf pred_ msgfct (u act))

-- | Unlifted 'F.throwIf_'.
--
-- @since 0.2.4.0
{-# INLINE throwIf_ #-}
throwIf_ :: MonadUnliftIO m => (a -> Bool) -> (a -> String) -> m a -> m ()
throwIf_ pred_ msgfct act = withRunInIO (\u -> F.throwIf_ pred_ msgfct (u act))

-- | Unlifted 'F.throwIfNeg'.
--
-- @since 0.2.4.0
{-# INLINE throwIfNeg #-}
throwIfNeg :: (MonadUnliftIO m, Ord a, Num a) => (a -> String) -> m a -> m a
throwIfNeg msgfct act = withRunInIO (\u -> F.throwIfNeg msgfct (u act))

-- | Unlifted 'F.throwIfNeg_'.
--
-- @since 0.2.4.0
{-# INLINE throwIfNeg_ #-}
throwIfNeg_ :: (MonadUnliftIO m, Ord a, Num a) => (a -> String) -> m a -> m ()
throwIfNeg_ msgfct act = withRunInIO (\u -> F.throwIfNeg_ msgfct (u act))

-- | Unlifted 'F.throwIfNull'.
--
-- @since 0.2.4.0
{-# INLINE throwIfNull #-}
throwIfNull :: MonadUnliftIO m => String -> m (Ptr a) -> m (Ptr a)
throwIfNull msg act = withRunInIO (\u -> F.throwIfNull msg (u act))

-- | Lifted 'F.newPool'.
--
-- @since 0.2.4.0
{-# INLINE newPool #-}
newPool :: MonadIO m => m Pool
newPool = liftIO F.newPool

-- | Lifted 'F.freePool'.
--
-- @since 0.2.4.0
{-# INLINE freePool #-}
freePool :: MonadIO m => Pool -> m ()
freePool = liftIO . F.freePool

-- | Unlifted 'F.withPool'.
--
-- @since 0.2.4.0
{-# INLINE withPool #-}
withPool :: MonadUnliftIO m => (Pool -> m b) -> m b
withPool act = withRunInIO (\u -> F.withPool (u . act))

-- | Lifted 'F.pooledMalloc'.
--
-- @since 0.2.4.0
{-# INLINE pooledMalloc #-}
pooledMalloc :: (MonadIO m, Storable a) => Pool -> m (Ptr a)
pooledMalloc = liftIO . F.pooledMalloc

-- | Lifted 'F.pooledMallocBytes'.
--
-- @since 0.2.4.0
{-# INLINE pooledMallocBytes #-}
pooledMallocBytes :: MonadIO m => Pool -> Int -> m (Ptr a)
pooledMallocBytes pool size = liftIO (F.pooledMallocBytes pool size)

-- | Lifted 'F.pooledRealloc'.
--
-- @since 0.2.4.0
{-# INLINE pooledRealloc #-}
pooledRealloc :: (MonadIO m, Storable a) => Pool -> Ptr a -> m (Ptr a)
pooledRealloc pool ptr = liftIO (F.pooledRealloc pool ptr)

-- | Lifted 'F.pooledReallocBytes'.
--
-- @since 0.2.4.0
{-# INLINE pooledReallocBytes #-}
pooledReallocBytes :: MonadIO m => Pool -> Ptr a -> Int -> m (Ptr a)
pooledReallocBytes pool ptr size = liftIO (F.pooledReallocBytes pool ptr size)

-- | Lifted 'F.pooledMallocArray'.
--
-- @since 0.2.4.0
{-# INLINE pooledMallocArray #-}
pooledMallocArray :: (MonadIO m, Storable a) => Pool -> Int -> m (Ptr a)
pooledMallocArray pool size = liftIO (F.pooledMallocArray pool size)

-- | Lifted 'F.pooledMallocArray0'.
--
-- @since 0.2.4.0
{-# INLINE pooledMallocArray0 #-}
pooledMallocArray0 :: (MonadIO m, Storable a) => Pool -> Int -> m (Ptr a)
pooledMallocArray0 pool size = liftIO (F.pooledMallocArray0 pool size)

-- | Lifted 'F.pooledReallocArray'.
--
-- @since 0.2.4.0
{-# INLINE pooledReallocArray #-}
pooledReallocArray ::
     (MonadIO m, Storable a) => Pool -> Ptr a -> Int -> m (Ptr a)
pooledReallocArray pool ptr size = liftIO (F.pooledReallocArray pool ptr size)

-- | Lifted 'F.pooledReallocArray0'.
--
-- @since 0.2.4.0
{-# INLINE pooledReallocArray0 #-}
pooledReallocArray0 ::
     (MonadIO m, Storable a) => Pool -> Ptr a -> Int -> m (Ptr a)
pooledReallocArray0 pool ptr size = liftIO (F.pooledReallocArray0 pool ptr size)

-- | Lifted 'F.pooledNew'.
--
-- @since 0.2.4.0
{-# INLINE pooledNew #-}
pooledNew :: (MonadIO m, Storable a) => Pool -> a -> m (Ptr a)
pooledNew pool val = liftIO (F.pooledNew pool val)

-- | Lifted 'F.pooledNewArray'.
--
-- @since 0.2.4.0
{-# INLINE pooledNewArray #-}
pooledNewArray :: (MonadIO m, Storable a) => Pool -> [a] -> m (Ptr a)
pooledNewArray pool vals = liftIO (F.pooledNewArray pool vals)

-- | Lifted 'F.pooledNewArray0'.
--
-- @since 0.2.4.0
{-# INLINE pooledNewArray0 #-}
pooledNewArray0 :: (MonadIO m, Storable a) => Pool -> a -> [a] -> m (Ptr a)
pooledNewArray0 pool marker vals = liftIO (F.pooledNewArray0 pool marker vals)

-- | Unlifted 'F.with'.
--
-- @since 0.2.4.0
{-# INLINE with #-}
with :: (MonadUnliftIO m, Storable a) => a -> (Ptr a -> m b) -> m b
with val f = withRunInIO (\u -> F.with val (u . f))

-- | Lifted 'F.new'.
--
-- @since 0.2.4.0
{-# INLINE new #-}
new :: (MonadIO m, Storable a) => a -> m (Ptr a)
new = liftIO . F.new

-- | Lifted 'F.maybeNew'.
--
-- @since 0.2.4.0
{-# INLINE maybeNew #-}
maybeNew :: MonadIO m => (a -> m (Ptr b)) -> Maybe a -> m (Ptr b)
maybeNew = maybe (return nullPtr)

-- | Lifted 'F.maybeWith'.
--
-- @since 0.2.4.0
{-# INLINE maybeWith #-}
maybeWith ::
     MonadIO m
  => (a -> (Ptr b -> m c) -> m c)
  -> Maybe a
  -> (Ptr b -> m c)
  -> m c
maybeWith = maybe ($ nullPtr)

-- | Unlifted 'F.maybePeek'.
--
-- @since 0.2.4.0
{-# INLINE maybePeek #-}
maybePeek :: MonadUnliftIO m => (Ptr a -> m b) -> Ptr a -> m (Maybe b)
maybePeek peek_ ptr = withRunInIO (\u -> F.maybePeek (u . peek_) ptr)

-- | Lifted 'F.copyBytes'.
--
-- @since 0.2.4.0
{-# INLINE copyBytes #-}
copyBytes :: MonadIO m => Ptr a -> Ptr a -> Int -> m ()
copyBytes dest src size = liftIO (F.copyBytes dest src size)

-- | Lifted 'F.moveBytes'.
--
-- @since 0.2.4.0
{-# INLINE moveBytes #-}
moveBytes :: MonadIO m => Ptr a -> Ptr a -> Int -> m ()
moveBytes dest src size = liftIO (F.moveBytes dest src size)

#if MIN_VERSION_base(4,8,0)
-- | Lifted 'F.fillBytes'.
--
-- @since 0.2.4.0
{-# INLINE fillBytes #-}
fillBytes :: MonadIO m => Ptr a -> Word8 -> Int -> m ()
fillBytes dest char size = liftIO (F.fillBytes dest char size)
#endif
