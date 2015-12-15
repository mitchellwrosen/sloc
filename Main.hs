{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Extra
import Data.ByteString.Char8   (ByteString)
import Data.Map                (Map)
import Data.Monoid
import Prelude                 hiding (lines)
import Streaming
import System.IO
import System.IO.Error
import System.Posix.ByteString (RawFilePath)
import Text.Printf

import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as List
import qualified Data.Map                as M
import qualified Streaming.Prelude       as S
import qualified System.Posix.ByteString as Posix

main :: IO ()
main = runResourceT (slocStream filenames) >>= prettyPrintSummary
  where
    filenames :: MonadResource m => Stream (Of RawFilePath) m ()
    filenames = dirtree ((/= '.') . BS.head) "."

    slocStream :: MonadResource m => Stream (Of RawFilePath) m r -> m (Map Filetype Int)
    slocStream = S.foldM_ step (pure mempty) pure
      where
        step :: MonadResource m => Map Filetype Int -> RawFilePath -> m (Map Filetype Int)
        step acc path = M.unionWith (+) acc <$> slocFile path

    slocFile :: MonadResource m => RawFilePath -> m (Map Filetype Int)
    slocFile path = fmap (M.singleton ft) $
          S.length_
        . S.filter (not . BS.null)
        . filterBlocks (isBeginBlockComment ft) (isEndBlockComment ft)
        . S.filter (not . isLineComment ft)
        $ lines path
      where
        ft :: Filetype
        ft = filetype path

    prettyPrintSummary :: Map Filetype Int -> IO ()
    prettyPrintSummary =
        mapM_ (\(lang, n) -> printf "%7d %s\n" n (show lang))
      . List.sortBy (\x y -> snd y `compare` snd x)
      . M.toList

-- | Filter blocks of consecutive elements out of a stream, given "begin block"
-- and "end block" predicates.
filterBlocks
    :: (Eq a, Monad m)
    => (a -> Bool)
    -> (a -> Bool)
    -> Stream (Of a) m r
    -> Stream (Of a) m r
filterBlocks isBegin isEnd s0 = do
    s1 <- S.break isBegin s0
    s2 <- lift (S.effects (S.break isEnd s1))
    lift (S.next s2) >>= \case
        Left r        -> pure r
        Right (_, s3) -> filterBlocks isBegin isEnd s3

-- | Stream the files in a directory tree. Only emit files and recurse into
-- directories for which the predicate applied to the path returns true.
dirtree
    :: forall m. MonadResource m
    => (RawFilePath -> Bool) -- Predicate to emit file and recurse into dir
    -> RawFilePath
    -> Stream (Of RawFilePath) m ()
dirtree p path = bracketStream (Posix.openDirStream path) Posix.closeDirStream loop
  where
    loop :: Posix.DirStream -> Stream (Of RawFilePath) m ()
    loop s = do
        liftIO (Posix.readDirStream s) >>= \case
            "" -> pure ()
            "." -> loop s
            ".." -> loop s
            entry | p entry -> do
                let full_path = path <> "/" <> entry
                liftIO (isReadableDirectory full_path) >>= \case
                    True  -> dirtree p full_path
                    False -> S.yield full_path
                loop s
            _ -> loop s

-- | Stream the lines of a file.
lines :: forall m. MonadResource m => RawFilePath -> Stream (Of ByteString) m ()
lines path = bracketStream open hClose go
  where
    open :: IO Handle
    open = Posix.openFd path Posix.ReadOnly Nothing Posix.defaultFileFlags
             >>= Posix.fdToHandle

    go :: Handle -> Stream (Of ByteString) m ()
    go h = do
        eof <- liftIO (hIsEOF h)
        unless eof (do
            line <- liftIO (BS.hGetLine h)
            S.yield line
            go h)

--------------------------------------------------------------------------------
-- ByteString versions of random filesystem functions.

isReadableDirectory :: RawFilePath -> IO Bool
isReadableDirectory path =
    doesDirectoryExist path &&^ Posix.fileAccess path True False True

doesDirectoryExist :: RawFilePath -> IO Bool
doesDirectoryExist path = fmap Posix.isDirectory (Posix.getFileStatus path)
    `catchIOError` \_ -> pure False

--------------------------------------------------------------------------------
-- Filetype ADT and friends.

data Filetype
    = Haskell
    | CPlusPlus
    | Unknown
    deriving (Eq, Ord, Show)

filetype :: RawFilePath -> Filetype
filetype path
    | ".hs"  `BS.isSuffixOf` path = Haskell
    | ".cpp" `BS.isSuffixOf` path = CPlusPlus
    | otherwise                   = Unknown

isLineComment :: Filetype -> ByteString -> Bool
isLineComment Haskell   = BS.isPrefixOf "--"
isLineComment CPlusPlus = BS.isPrefixOf "//"
isLineComment _         = const False

isBeginBlockComment :: Filetype -> ByteString -> Bool
isBeginBlockComment Haskell   = BS.isPrefixOf "{-"
isBeginBlockComment CPlusPlus = BS.isPrefixOf "/*"
isBeginBlockComment _         = const False

isEndBlockComment :: Filetype -> ByteString -> Bool
isEndBlockComment Haskell   = BS.isSuffixOf "-}"
isEndBlockComment CPlusPlus = BS.isSuffixOf "*/"
isEndBlockComment _         = const False
