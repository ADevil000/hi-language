{-# LANGUAGE DeriveFunctor #-}
module HW3.Action where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Control.Monad
import qualified Data.ByteString as B
import Data.Functor
import qualified Data.Sequence as S
import Data.Set (Set, notMember)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock
import HW3.Base
import System.Directory
import System.Random

-- | Data that present permissions for actions
data HiPermission =
    AllowRead  -- ^ Permission for reading
  | AllowWrite  -- ^ Permissiong for writing
  | AllowTime -- ^ Permission for getting time
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Data of exceptions for actions that doesn't have special permission
data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

-- | Instance of exception for PermissionException
instance Exception PermissionException

-- | Data HIO that has only runHIO function
newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor)

-- | Monad instance for HIO
instance Monad HIO where
  return a = HIO { runHIO = const $ return a }
  m >>= f = HIO { runHIO = \w ->
    do
      io <- runHIO m w
      runHIO (f io) w
  }

-- | Applicative instance for HIO by using Monad instance
instance Applicative HIO where
  (<*>) = ap
  pure = return

-- | HiMonad instance for HIO.
instance HiMonad HIO where
  -- | Realisation of cwd action. Need read permission. Return path in HiValueString
  runAction HiActionCwd = HIO { runHIO = \sp ->
    if notMember AllowRead sp
    then throwIO $ PermissionRequired AllowRead
    else getCurrentDirectory <&> (HiValueString . T.pack)
  }

  -- | Realisation of read action. Need read permission.
  -- If given filepath is dir than return list of content of this dir,
  -- else try read content of the file with given name and return content of file.
  runAction (HiActionRead fp) = HIO { runHIO = \sp ->
    if notMember AllowRead sp
    then throwIO $ PermissionRequired AllowRead
    else do
      isFile <- doesDirectoryExist fp
      if isFile
      then listDirectory fp <&> (HiValueList . S.fromList . map (HiValueString . T.pack))
      else B.readFile fp <&> bsToHiVal
  }
    where
      -- | If given sequence of bytes is string then return string presentation, else return given bytes
      bsToHiVal :: B.ByteString -> HiValue
      bsToHiVal bs = case decodeUtf8' bs of
                          Left _    -> HiValueBytes bs
                          Right str -> HiValueString str

  -- | Realisation of write action. Need write permission. Return HiValueNull
  runAction (HiActionWrite fp bs) = HIO { runHIO = \sp ->
    if notMember AllowWrite sp
    then throwIO $ PermissionRequired AllowWrite
    else B.writeFile fp bs >> return HiValueNull
  }

  -- | Realisation of mkdir action. Need write permission. If given dir exist, than change with new empty.
  -- Return HivalueNull
  runAction (HiActionMkDir fp) = HIO { runHIO = \sp ->
    if notMember AllowWrite sp
    then throwIO $ PermissionRequired AllowWrite
    else createDirectoryIfMissing True fp >> return HiValueNull
  }

  -- | Realisation of cd action. Need read permission. Return HiValueNull
  runAction (HiActionChDir fp) = HIO { runHIO = \sp ->
    if notMember AllowRead sp
    then throwIO $ PermissionRequired AllowRead
    else setCurrentDirectory fp >> return HiValueNull
  }

  -- | Realisation of now action. Need time permission. Return HiValueTime
  runAction HiActionNow = HIO { runHIO = \sp ->
    if notMember AllowTime sp
    then throwIO $ PermissionRequired AllowTime
    else getCurrentTime <&> HiValueTime
  }

  -- | Realisation of rand action. Return HiValueNumber
  runAction (HiActionRand l r) = HIO { runHIO = \_ -> getStdRandom (uniformR (l, r))  <&> HiValueNumber . toRational }

  -- | Realisation of echo action. Need write permission. Return HiValueNull
  runAction (HiActionEcho s) = HIO { runHIO = \sp ->
    if notMember AllowWrite sp
    then throwIO $ PermissionRequired AllowWrite
    else do
      _ <- putStrLn $ T.unpack s
      return HiValueNull
  }
