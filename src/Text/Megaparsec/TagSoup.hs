{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
-- |
-- Module      :  Text.Megaparsec.TagSoup
-- Copyright   :  © 2016 Kwang Yul Seo
-- License     :  BSD
--
-- Maintainer  :  Kwang Yul Seo <kwangyul.seo@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Make Tags an instance of 'Stream' with 'Tag str' token type.
module Text.Megaparsec.TagSoup
  ( TagParser
  , space
  , whitespace
  , lexeme
  , satisfy
  , anyTag
  , anyTagOpen
  , anyTagClose
  , tagText
  , tagOpen
  , tagClose
  ) where

import Data.Char (isSpace)
import Data.Foldable
import Data.Semigroup ((<>))
import qualified Data.Set as S
import Control.Monad.Combinators
import Text.HTML.TagSoup
import Text.StringLike
import Text.Megaparsec.Pos
import Text.Megaparsec

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for TagSoup tags.
type TagParser e str = Parsec e [Tag str]

incPos :: Pos -> Pos
incPos = (pos1 <>)

instance forall str. (Ord str, Show str) => Stream [Tag str] where
  type Token [Tag str] = Tag str
  type Tokens [Tag str] = [Tag str]
  tokenToChunk _ tag = [tag]
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ [] = True
  chunkEmpty _ _  = False
  take1_ [] = Nothing
  take1_ (x:xs) = Just (x, xs)
  takeN_ i s
    | i <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (take i s, drop i s)
  takeWhile_ f = takeWhile' [] where
    takeWhile' ys [] = (ys, [])
    takeWhile' ys (x:xs)
      | f x = takeWhile' (x:ys) xs
      | otherwise = (ys, x:xs)
  showTokens _ = show
  reachOffset i state = reachOffset' i state [] where
    replaceEmpty "" = "<empty line>"
    replaceEmpty x = x
    reachOffset' :: Int -> PosState [Tag str] -> [String] -> (SourcePos, String, PosState [Tag str])
    reachOffset' j s' strs
      | j <= 0 = (pstateSourcePos s', replaceEmpty (fold (reverse strs)), s')
      | otherwise = let str = case pstateInput s' of
                                [] -> ""
                                (x:_) -> show x
                    in reachOffset' (j - 1) (incState state) (str:strs) where
          incState PosState {..} = let SourcePos {..} = pstateSourcePos
                                       nspos = SourcePos { sourceColumn = incPos sourceColumn, .. }
                                     in
            PosState { pstateOffset = pstateOffset + 1, pstateSourcePos = nspos, .. }

-- | Parses a text block containing only characters which satisfy 'isSpace'.
space :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
space = satisfy (\case TagText x | all isSpace (toString x) -> True
                       TagComment _ -> True
                       _ -> False)

-- | Parses any whitespace. Whitespace consists of zero or more ocurrences of 'space'.
whitespace :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m ()
whitespace = skipMany space

-- | @lexeme p@ first applies parser @p@ and then 'whitespace', returning the value of @p@.
--
--    @lexeme = (<* whitespace)@
--
--   Every tag parser is defined using 'lexeme', this way every parse starts at a point
--   without whitespace.
--
--   The only point where 'whitespace' should be called explicitly is at the start of
--   the top level parser, in order to skip any leading whitespace.
lexeme :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str) -> m (Tag str)
lexeme p = p <* whitespace

-- | Parses any tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTag :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
anyTag = lexeme $ token Just S.empty -- Passing "Just" means the parser will always
                                     -- succeed so it does not matter what the set is.

-- | Parse any opening tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagOpen :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
anyTagOpen = lexeme (satisfy isTagOpen <?> "TagOpen")

-- | Parse any closing tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagClose :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
anyTagClose = lexeme (satisfy isTagClose <?> "TagClose")

-- | Parses a chunk of text.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
tagText :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
tagText = lexeme (satisfy isTagText <?> "TagText")

-- | Parse the given opening tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagOpen :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => str -> m (Tag str)
tagOpen s = lexeme (satisfy (isTagOpenName s) <?> "TagOpen " ++ (toString s))

-- | Parse the given closing tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagClose :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => str -> m (Tag str)
tagClose s = lexeme (satisfy (isTagCloseName s) <?> "TagClose " ++ (toString s))

