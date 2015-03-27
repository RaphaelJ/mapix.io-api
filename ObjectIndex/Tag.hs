module ObjectIndex.Tag (
      tagPath, tagPathParser, tagPath2Text
    -- * Conditional tag expressions.
    , tagExpressionParser, getMatchingObjects
    ) where

import Prelude

import Control.Applicative ((<$>), (<*), (<*>))
import Data.Set (Set)
import Data.Text (Text)
import Text.Parsec
import Text.Parsec.Text

import qualified Data.Set   as S
import qualified Data.Text  as T

import ObjectIndex.Manage (getTagObjects, lookupTag)
import ObjectIndex.Type

-- | Returns the full name of the tag (i.e. @theme:beach@).
tagPath :: Tag -> TagPath
tagPath =
    go []
  where
    go acc (Tag RootTag              _ _) = TagPath acc
    go acc (Tag (SubTag name parent) _ _) = go (name : acc) parent

-- | Parses a tag path (i.e. @theme:beach@).
tagPathParser :: Parser TagPath
tagPathParser = let tagName = T.pack <$> many1 (lower <|> digit)
                in TagPath <$> tagName `sepBy1` char ':'

tagPath2Text :: TagPath -> Text
tagPath2Text = T.intercalate ":" . tpNodes

-- Tag expressions -------------------------------------------------------------

-- | Parses a conditional tag expression (i.e. @theme:beach || theme:ocean@).
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
        <|> (TagName <$> (tagPathParser <* spaces))

-- | Searches for objects in the user's index which match the given tag
-- expression. If no 'TagExpression' is given, return the 'RootTag' objects.
-- Returns an empty set if the tag doesn't exist.
getMatchingObjects :: UserIndex -> Maybe TagExpression
                  -> IndexSTM (Set IndexedObject)
getMatchingObjects ui Nothing     = getTagObjects (uiRootTag ui)
getMatchingObjects ui (Just expr) =
    go expr
  where
    go (TagName path) = do
        mTag <- lookupTag ui path
        case mTag of Just tag -> getTagObjects tag
                     Nothing  -> return S.empty
    go (TagNot expr1) = S.difference <$> getTagObjects (uiRootTag ui)
                                     <*> go expr1
    go (TagAnd expr1 expr2) = S.intersection <$> go expr1 <*> go expr2
    go (TagOr  expr1 expr2) = S.union        <$> go expr1 <*> go expr2
