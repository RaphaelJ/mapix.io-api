module Handler.Tag (
      TagExpression (..), tagGetParam
    , getTagsR, getTagR
    , tagExpressionParser, tagPathParser , tagPath
    , getTagExpressionn, getMatchingImages
    ) where

import Import

import Prelude
import Control.Applicative ((<$>), (<*))
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (fromText, toLazyText)
import Text.Parsec
import Text.Parsec.Text

import ImageIndex.Manage (getTagImages)
import ImageIndex.Type

data TagExpression = TagPath TagPath
                   | TagNot  TagExpression
                   | TagAnd  TagExpression TagExpression
                   | TagOr   TagExpression TagExpression
    deriving Show

-- Handlers --------------------------------------------------------------------

-- | Returns the hierarchy of tags.
getTagsR :: Handler Html
getTagsR = do
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    rootTag <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        uiRootTag ui

    returnJson rootTag

-- | Returns the tag and its sub-tags. Fails with a '404 Not Found' error if the
-- tag doesn't exist.
getTagR :: TagPath -> Handler Html
getTagR path =
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    mTag <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        lookupTag ui path

    case mTag of Just tag -> returnJson rootTag
                 _        -> notFound

-- | Removes this tag and its subtags without removing the images. Returns a
-- '204 No content' on success. Fails with a '404 Not Found' error if the tag
-- doesn't exist.
deleteTagR :: TagPath -> Handler Html
deleteTagR path =
    username    <- mhUser <$> getMashapeHeaders
    ii          <- imageIndex <$> getYesod
    currentTime <- lift getCurrentTime

    exists <- atomically $ do
        ui <- getUserIndex ii userName currentTime
        mTag <- lookupTag ui path
        case mTag of Nothing  -> return False
                     Just tag -> removeTag tag >> return True

    if exists then sendResponseStatus noContent204 ()
              else notFound

-- Expressions -----------------------------------------------------------------

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
                in TagPath <$> tagName `sepBy1` char ':'

-- | Returns the full name of the tag (i.e. @theme:beach@).
tagPath :: Tag -> TagPath
tagPath =
    go []
  where
    go acc (Tag RootTag              _ _) = acc
    go acc (Tag (SubTag name parent) _ _) = go (name : acc) parent

tagPath2Text = T.intercalate ":"

instance PathPiece TagPath where
    fromPathPiece txt =
        case parse tagPathParser "" txt of Right path -> Just path
                                           Left _     -> Nothing

    toPathPiece = tagPath2Text

-- Search ----------------------------------------------------------------------

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
