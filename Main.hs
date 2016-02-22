{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Streaming.Filesystem

import Control.Monad.Extra
import Data.ByteString.Char8   (ByteString)
import Data.Map                (Map)
import Prelude                 hiding (lines, readFile)
import Streaming
import System.Posix.ByteString (RawFilePath)
import Text.Printf

import qualified Data.ByteString.Char8 as BS
import qualified Data.List             as List
import qualified Data.Map              as M
import qualified Streaming.Prelude     as S

main :: IO ()
main = runResourceT (S.mconcat_ (S.mapM sloc filenames)) >>= prettyPrintSummary
 where
  filenames :: MonadResource m => Stream (Of RawFilePath) m ()
  filenames = dirtree (pure . (/= '.') . BS.head) "."

  sloc :: MonadResource m => RawFilePath -> m (Map Filetype Int)
  sloc path = fmap (M.singleton ft) $
        S.length_
      . filterBlocks (isBeginBlockComment ft) (isEndBlockComment ft)
      . S.filter (not . isLineComment ft)
      . S.filter (not . BS.null)
      $ readFile path
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
