# tagsoup-megaparsec

[![Build Status](https://travis-ci.org/kseo/tagsoup-megaparsec.svg?branch=master)](https://travis-ci.org/kseo/tagsoup-megaparsec)

A Tag token parser and Tag specific parsing combinators, inspired by [parsec-tagsoup][parsec-tagsoup] and [tagsoup-parsec][tagsoup-parsec]. This library helps you build a megaparsec parser using TagSoup's Tag as tokens.

[parsec-tagsoup]: https://hackage.haskell.org/package/parsec-tagsoup
[tagsoup-parsec]: https://hackage.haskell.org/package/tagsoup-parsec

## Usage

### DOM parser

We can build a DOM parser using TagSoup's Tag as a token type in Megaparsec. Let's start the example with importing all the required modules.

```haskell
import Data.Text ( Text )
import qualified Data.Text as T
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HMS
import Text.HTML.TagSoup
import Text.Megaparsec
import Text.Megaparsec.ShowToken
import Text.Megaparsec.TagSoup
```

Here's the data types used to represent our DOM. `Node` is either `ElementNode` or `TextNode`. `TextNode` data constructor takes a `Text` and `ElementNode` data constructor takes an `Element` whose fields consist of `elementName`, `elementAttrs` and `elementChildren`.

```haskell
type AttrName   = Text
type AttrValue  = Text

data Element = Element
  { elementName :: !Text
  , elementAttrs :: !(HashMap AttrName AttrValue)
  , elementChildren :: [Node]
  } deriving (Eq, Show)

data Node =
    ElementNode Element
  | TextNode Text
  deriving (Eq, Show)
```

Our `Parser` is defined as a type synonym for `TagParser Text`. `TagParser` takes a type argument representing the string type and we chose `Text` here. We can pass any of `StringLike` types such as `String` and `ByteString`.

```haskell
type Parser = TagParser Text
```

There is nothing new in defining a parser except that our token is `Tag Text` instead of `Char`. We can use any Megaparsec combinators we want as usual. Our `node` parser is either `element` or `text` so we used the choice combinator `(<|>)`.

```haskell
node :: Parser Node
node = ElementNode <$> element
   <|> TextNode <$> text
```

tagsoup-megaparsec library provides some `Tag` specific combinators.

* `tagText`: parse a chunk of text.
* `anyTagOpen`/`anyTagClose`: parse any opening and closing tag.

`text` and `element` parsers are built using these combinators.

NOTE: We don't need to worry about the text blocks containing only whitespace characters because all the parsers provided by tagsoup-megaparsec are lexeme parsers.

```haskell
text :: Parser Text
text = fromTagText <$> tagText

element :: Parser Element
element = do
  t@(TagOpen tagName attrs) <- anyTagOpen
  children <- many node
  closeTag@(TagClose tagName') <- anyTagClose
  if tagName == tagName'
     then return $ Element tagName (HMS.fromList attrs) children
     else fail $ "unexpected close tag" ++ showToken closeTag
```

Now it's time to define our driver. `parseDOM` takes a `Text` and returns either `ParseError` or `[Node]`. We used `many` combinator to represent that there are zero or more occurences of `node`. We used TagSoup's `parseTags` to create tokens and passed it to Megaparsec's `parse` function.

```haskell
parseDOM :: Text -> Either ParseError [Node]
parseDOM html = parse (many node) "" tags
  where tags = parseTags html
```

