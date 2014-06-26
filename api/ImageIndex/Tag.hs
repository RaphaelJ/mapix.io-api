-- | Provides functions to interpret tag expressions.
module ImageIndex.Tag (
      TagExpression (..)
    , tagGetParam, tagExpressionParser, tagPathParser , tagPath
    , getTagExpressionn, getMatchingImages
    ) where

import Prelude
import Control.Applicative ((<$>), (<*))
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Text.Parsec
import Text.Parsec.Text

import ImageIndex.Manage (getTagImages)
import ImageIndex.Type

-- | Name of the GET variable which is used in queries to filter images by tag.
tagGetParam :: Text
tagGetParam = "tag"

-- Expressions -----------------------------------------------------------------

data TagExpression = TagPath TagPath
                   | TagNot  TagExpression
                   | TagAnd  TagExpression TagExpression
                   | TagOr   TagExpression TagExpression
    deriving Show

-- | Parses a conditionnal tag expression (i.e. @theme:beach || theme:ocean@).
tagExpressionParser :: Parser TagExpression
tagExpressionParser =
    spaces >> orExpr
  where
    orExpr = do
        left <- andExpr
        (    (string "||" >> spaces >> (TagOr left <$> orExpr))
         <|> return left)

    andExpr = do
        left <- expr
        (    (string "&&" >> spaces >> (TagAnd left <$> andExpr))
         <|> return left)

    expr = (char '!' >> spaces >> do
                subExpr <- expr
                case subExpr of
                    TagNot subExpr' -> return subExpr'
                    _               -> return $ TagNot subExpr)
        <|> between (char '(' >> spaces) (char ')' >> spaces) orExpr
        <|> (TagPath <$> (tagPathParser <* spaces))

-- | Parses one tag expression (i.e. @theme:beach@).
tagPathParser :: Parser TagPath
tagPathParser = let tagName = T.pack <$> many1 (lower <|> digit)
                in tagName `sepBy1` char ':'

tagListParser :: Parser [TagPath]
tagListParser =
    let separator = spaces >> char ',' >> spaces
    in spaces >> (tagPathParser `sepBy` separator)

-- | Returs the full name of the tag (i.e. @theme:beach@).
tagPath :: Tag -> Text
tagPath =
    toStrict . toLazyText . go ""
  where
    go acc (Tag RootTag              _ _) = acc
    go acc (Tag (SubTag name parent) _ _) =
        go (":" <> fromText name <> acc) parent

-- Search ----------------------------------------------------------------------

-- | Tries to parse the tag GET parameter. Returns 'Nothing' if no tag parameter
-- has been given. Fails with an API error if the tag expression failed to be
-- parsed.
getTagExpression :: Handler (Maybe TagExpression)
getTagExpression = do
    mExpr <- lookupGetParams tagExprGetParam
    case mExpr of Just expr -> parseExpr expr
                  Nothing   -> Nothing
  where
    parseExpr expr = case parse tagExpressionParser "" expr of
                        Left  err  -> apiFail (InvalidTagExpression err)
                        Right expr -> Just expr

-- | Searchs for images in the user\'s index which match the given tag
-- expression. If no 'TagExpression' is given, return the 'RootTag' images.
-- Returns an empty set if the tag doesn't exist.
getMatchingImages :: UserIndex -> Maybe TagExpression -> STM (Set Image)
getMatchingImages ui Nothing     = getTagImages (uiRootTag ui)
getMatchingImages ui (Just expr) =
    go expr
  where
    go (TagPath path) = do
        mTag <- lookupTag ui tagPath
        case mTag of Just tag -> getTagImages tag
                     Nothing  -> return S.empty
    go (TagNot expr1) = S.difference <*> getTagImages (uiRootTag ui)
                                     <*> go expr1
    go (TagAnd expr1 expr2) = S.intersection <$> go expr1 <*> go expr2
    go (TagOr  expr1 expr2) = S.union        <$> go expr1 <*> go expr2

-- New image -------------------------------------------------------------------


