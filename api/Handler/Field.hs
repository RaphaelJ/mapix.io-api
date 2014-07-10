module Handler.Field (
      
    ) where

countField = checkBool (> 0) "Non-positive count value" intField