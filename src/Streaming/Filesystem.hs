{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streaming.Filesystem where

import Control.Monad.Extra
import Data.ByteString (ByteString)
import Data.Monoid
import Streaming
import System.IO (Handle, hClose, hIsEOF)
import System.IO.Error
import System.Posix.ByteString

import qualified Data.ByteString   as BS
import qualified Streaming.Prelude as S

-- | Stream the lines of a file.
readFile :: forall m. MonadResource m => RawFilePath -> Stream (Of ByteString) m ()
readFile path = bracketStream open hClose go
 where
  open :: IO Handle
  open = openFd path ReadOnly Nothing defaultFileFlags >>= fdToHandle

  go :: Handle -> Stream (Of ByteString) m ()
  go h =
    unlessM (liftIO (hIsEOF h)) $ do
      line <- liftIO (BS.hGetLine h)
      S.yield line
      go h

dirwalk :: forall m. MonadResource m => RawFilePath -> Stream (Of RawFilePath) m ()
dirwalk path = bracketStream (openDirStream path) closeDirStream loop
 where
  loop :: DirStream -> Stream (Of RawFilePath) m ()
  loop s = liftIO (readDirStream s) >>= \case
    ""    -> pure ()
    "."   -> loop s
    ".."  -> loop s
    entry -> do
      S.yield entry
      loop s

-- | Stream the files in a directory tree. Only emit files and recurse into
-- directories for which the predicate applied to the path returns true.
dirtree
  :: forall m. MonadResource m
  => (RawFilePath -> m Bool) -- Predicate to emit file and recurse into dir
  -> RawFilePath
  -> Stream (Of RawFilePath) m ()
dirtree p dir = S.for (dirwalk dir) f
 where
  f :: RawFilePath -> Stream (Of RawFilePath) m ()
  f path = do
    let full_path = dir <> "/" <> path
    whenM (lift (p path)) $ do
      is_dir <- liftIO (isReadableDirectory full_path)
      if is_dir
        then dirtree p full_path
        else S.yield full_path

isReadableDirectory :: RawFilePath -> IO Bool
isReadableDirectory path =
  doesDirectoryExist path &&^ fileAccess path True False True

doesDirectoryExist :: RawFilePath -> IO Bool
doesDirectoryExist path =
  fmap isDirectory (getFileStatus path)
    `catchIOError` \_ -> pure False

