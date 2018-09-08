{-# LANGUAGE QuasiQuotes #-}
import Data.Void
import Test.Hspec
import Text.RawString.QQ
import Text.HTML.TagSoup
import Text.Megaparsec
import Text.Megaparsec.TagSoup

type StringTagParser = TagParser Void String

testDoc = [r|
<ul>
<li>Item 1</li>
<!-- comments are ignored -->
<li>Item 2</li>
</ul>
|]

liParser :: StringTagParser String
liParser = do
  tagOpen "li"
  text <- tagText
  tagClose "li"
  return (fromTagText text)

ulParser :: StringTagParser [String]
ulParser = do
  tagOpen "ul"
  texts <- many liParser
  tagClose "ul"
  return texts

tagParserSpec :: Spec
tagParserSpec = do
  let input = parseTags testDoc
  describe "TagParser" $ do
    it "parses a xml doc" $
      parse (whitespace *> ulParser) "" input `shouldBe` Right ["Item 1", "Item 2"]

main :: IO ()
main = hspec tagParserSpec
