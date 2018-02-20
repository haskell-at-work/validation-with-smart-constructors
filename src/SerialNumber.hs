module SerialNumber
  ( SerialNumber
  , ValidationError(..)
  , makeSerialNumber
  , renderSerialNumber
  ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import qualified Data.Text as Text

newtype SerialNumber =
  SerialNumber [Text]
  deriving (Show, Eq)

data ValidationError
  = WrongNumberOfGroups Int
  | InvalidGroupLength Int Text
  | InvalidCharacters (HashSet Char)
  deriving (Show, Eq)

validChars :: HashSet Char
validChars = HashSet.fromList (['A' .. 'Z'] ++ ['0' .. '9'])

separator :: Text
separator = Text.singleton '-'

makeSerialNumber :: Text -> Either ValidationError SerialNumber
makeSerialNumber t = do
  gs <- mapM validateGroup (Text.splitOn separator t)
  if length gs == 4
    then Right (SerialNumber gs)
    else Left (WrongNumberOfGroups (length gs))
  where
    validateGroup group
      | len /= 4 = Left (InvalidGroupLength len group)
      | not (HashSet.null invalidChars) = Left (InvalidCharacters invalidChars)
      | otherwise = Right group
      where
        len = Text.length group
        invalidChars =
          HashSet.difference (HashSet.fromList (Text.unpack group)) validChars

renderSerialNumber :: SerialNumber -> Text
renderSerialNumber (SerialNumber groups) =
  Text.intercalate separator groups
