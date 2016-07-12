{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
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
import Data.List (intercalate)
import Text.HTML.TagSoup
import Text.StringLike
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for TagSoup tags.
type TagParser str = Parsec [Tag str]

instance (Show str) => ShowToken (Tag str) where
    showToken tag = show tag

instance (ShowToken (Tag str)) => ShowToken [Tag str] where
    showToken tags = intercalate " " (map showToken tags)

updatePosTag :: Int         -- ^ Tab width
             -> SourcePos   -- ^ Initial position
             -> (Tag str)   -- ^ Tag at the position
             -> SourcePos
updatePosTag width sourcePos tag = incSourceColumn sourcePos 1

-- | Parses a text block containing only characters which satisfy 'isSpace'.
space :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str)
space = satisfy (\tag -> case tag of
                           TagText x | all isSpace (toString x) -> True
                           _ -> False)

-- | Parses any whitespace. Whitespace consists of zero or more ocurrences of 'space'.
whitespace :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m ()
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
lexeme :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str) -> m (Tag str)
lexeme p = p <* whitespace

-- | Parses any tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTag :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str)
anyTag = lexeme $ token updatePosTag Right

-- | Parse a tag if it satisfies the predicate.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
satisfy :: (Show str, StringLike str, MonadParsec s m (Tag str)) => (Tag str -> Bool) -> m (Tag str)
satisfy f = lexeme $ token updatePosTag testTag
  where testTag x = if f x
                       then Right x
                       else Left . pure . Unexpected . showToken $ x

-- | Parse any opening tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagOpen :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str)
anyTagOpen = satisfy isTagOpen <?> "any tag open"

-- | Parse any closing tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagClose :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str)
anyTagClose = satisfy isTagClose <?> "any tag close"

-- | Parses a chunk of text.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
tagText :: (Show str, StringLike str, MonadParsec s m (Tag str)) => m (Tag str)
tagText = satisfy isTagText <?> "text"

-- | Parse the given opening tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagOpen :: (Show str, StringLike str, MonadParsec s m (Tag str)) => str -> m (Tag str)
tagOpen s = satisfy (isTagOpenName s) <?> "tag open"

-- | Parse the given closing tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagClose :: (Show str, StringLike str, MonadParsec s m (Tag str)) => str -> m (Tag str)
tagClose s = satisfy (isTagCloseName s) <?> "tag close"

