{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}

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
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Text.HTML.TagSoup
import Text.StringLike
import Text.Megaparsec ((<|>))
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for TagSoup tags.
type TagParser str = Parsec Dec [Tag str]

instance (Show str) => ShowToken (Tag str) where
    showTokens tags = unwords (NE.toList (NE.map show tags))

instance (Ord str) => Stream [Tag str] where
  type Token [Tag str] = Tag str
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos = const updatePosTag
  {-# INLINE updatePos #-}

updatePosTag
  :: Pos                    -- ^ Tab width
  -> SourcePos              -- ^ Current position
  -> Tag str                -- ^ Current token
  -> (SourcePos, SourcePos) -- ^ Actual position and incremented position
updatePosTag _ apos@(SourcePos n l c) _ = (apos, npos)
  where
    u = unsafePos 1
    npos = SourcePos n l (c <> u)

-- | Parses a text block containing only characters which satisfy 'isSpace'.
space :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
space = satisfy (\tag -> case tag of
                           TagText x | all isSpace (toString x) -> True
                           _ -> False)

-- | Parses a comment.
comment :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
comment = satisfy (\tag -> case tag of
                             TagComment _ -> True
                             _ -> False)

-- | Parses any whitespace. Whitespace consists of zero or more
-- ocurrences of 'space' or 'comment'.
whitespace :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m ()
whitespace = skipMany (space <|> comment)

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
anyTag = lexeme $ token Right Nothing

-- | Parse a tag if it satisfies the predicate.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
satisfy :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => (Tag str -> Bool) -> m (Tag str)
satisfy f = lexeme $ token testTag Nothing
  where testTag x = if f x
                       then Right x
                       else Left (Set.singleton (Tokens (x:|[])), Set.empty, Set.empty)

-- | Parse any opening tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagOpen :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
anyTagOpen = satisfy isTagOpen <?> "any tag open"

-- | Parse any closing tag.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
anyTagClose :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
anyTagClose = satisfy isTagClose <?> "any tag close"

-- | Parses a chunk of text.
-- As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
tagText :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => m (Tag str)
tagText = satisfy isTagText <?> "text"

-- | Parse the given opening tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagOpen :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => str -> m (Tag str)
tagOpen s = satisfy (isTagOpenName s) <?> "tag open"

-- | Parse the given closing tag.
-- As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagClose :: (StringLike str, MonadParsec e s m, Token s ~ Tag str) => str -> m (Tag str)
tagClose s = satisfy (isTagCloseName s) <?> "tag close"
