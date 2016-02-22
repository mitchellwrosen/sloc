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
main = do
  counts <- runResourceT
              (S.foldM_ (\acc path -> (M.unionWith (+) acc) <$> sloc path)
                        (pure mempty)
                        pure
                        filenames)
  prettyPrintSummary counts
 where
  filenames :: MonadResource m => Stream (Of RawFilePath) m ()
  filenames = dirtree (pure . (/= '.') . BS.head) "."

  sloc :: MonadResource m => RawFilePath -> m (Map Filetype Int)
  sloc path = do
    len <-
        S.length_
      . filterBlocks (isBeginBlockComment ft) (isEndBlockComment ft)
      . S.filter (not . isLineComment ft)
      . S.filter (not . BS.null)
      $ readFile path

    pure (M.singleton ft len)
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
  = Cabal
  | CPlusPlus
  | Haskell
  | Markdown
  | Yaml
  | Unknown
  deriving (Eq, Ord, Show)

filetype :: RawFilePath -> Filetype
filetype path
  | ".cabal" `BS.isSuffixOf` path = Cabal
  | ".cpp"   `BS.isSuffixOf` path = CPlusPlus
  | ".hs"    `BS.isSuffixOf` path = Haskell
  | ".md"    `BS.isSuffixOf` path = Markdown
  | ".yaml"  `BS.isSuffixOf` path = Yaml
  | otherwise                    = Unknown

isLineComment :: Filetype -> ByteString -> Bool
isLineComment Cabal     = begin "--"
isLineComment CPlusPlus = begin "//"
isLineComment Markdown  = none
isLineComment Haskell   = begin "--"
isLineComment Yaml      = begin "#"
isLineComment Unknown   = none

isBeginBlockComment :: Filetype -> ByteString -> Bool
isBeginBlockComment Cabal     = none
isBeginBlockComment CPlusPlus = begin "/*"
isBeginBlockComment Markdown  = none
isBeginBlockComment Haskell   = begin "{-"
isBeginBlockComment Yaml      = none
isBeginBlockComment Unknown   = none

isEndBlockComment :: Filetype -> ByteString -> Bool
isEndBlockComment Cabal     = none
isEndBlockComment CPlusPlus = end "*/"
isEndBlockComment Markdown  = none
isEndBlockComment Haskell   = end "-}"
isEndBlockComment Yaml      = none
isEndBlockComment Unknown   = none

begin :: ByteString -> ByteString -> Bool
begin = BS.isPrefixOf

end :: ByteString -> ByteString -> Bool
end = BS.isSuffixOf

none :: ByteString -> Bool
none _ = False
