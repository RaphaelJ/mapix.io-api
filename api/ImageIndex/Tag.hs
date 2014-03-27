-- | Provides functions to interpret tag expressions.
module ImageIndex.Tag (
      tagGetParam, tagExpressionParser, tagPathParser, tagPath
    ) where

import Prelude
import Control.Applicative ((<$>), (<*))
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Parsec
import Text.Parsec.Text

import ImageIndex.Manage
import ImageIndex.Type

tagGetParam :: Text
tagGetParam = "tag"

lookupGetParams tagExprGetParam

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

-- | Returs the full name of the tag (i.e. @theme:beach@).
tagPath :: Tag -> TagPath
tagPath =
    toStrict . toLazyText . go ""
  where
    go acc (Tag RootTag              _ _) = acc
    go acc (Tag (SubTag name parent) _ _) =
        go (singleton ':' <> fromText name <> acc) parent

-- Search ----------------------------------------------------------------------

-- | Searchs for images in the user\'s index which matche the guven tag
-- expression.
matchingImages :: UserIndex -> TagExpression -> STM (Set Image)
matchingImages ui (TagPath tagPath)     = do
    mTag <- lookupTag ui tagPath
    case mTag of Just tag -> getTagImages tag
                 Nothing  -> return S.empty
matchingImages ui (TagNot  expr)        =
    S.difference <$> lookupTag ui [] <*> matchingImages ui expr
matchingImages ui (TagAnd  expr1 expr2) =
    S.intersection <$> matchingImages ui expr1 <*> matchingImages ui expr2
matchingImages ui (TagOr  expr1 expr2) =
    S.union <$> matchingImages ui expr1 <*> matchingImages ui expr2
