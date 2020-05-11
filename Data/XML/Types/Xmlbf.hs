{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.XML.Types.Xmlbf where

import Control.Applicative ((<|>))
import Data.Foldable (fold, foldMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as XML
import qualified HTMLEntities.Text as HTMLEntities
import qualified Xmlbf
import Xmlbf (FromXml (..), ToXml (..))

-------------
-- ToXml
-------------
instance ToXml XML.Document where
  toXml XML.Document {..} =
    toXml documentPrologue
      <> toXml documentRoot
      <> (toXml =<< documentEpilogue)

instance ToXml XML.Prologue where
  toXml XML.Prologue {..} =
    (toXml =<< prologueBefore)
      <> (toXml =<< maybeToList prologueDoctype)
      <> (toXml =<< prologueAfter)

instance ToXml XML.Miscellaneous where
  toXml = mempty

instance ToXml XML.Doctype where
  toXml = mempty

instance ToXml XML.Element where
  toXml XML.Element {..} =
    Xmlbf.element
      (xmlbfName elementName)
      (HashMap.fromList (fmap xmlbfAttr elementAttributes))
      (toXml =<< elementNodes)

-- | Turn a @(Name, Content)@ pair from Data.XML.Types into a @(Text,
--   Text)@ name, attribute pair for Xmlbf.
xmlbfAttr :: (XML.Name, [XML.Content]) -> (Text, Text)
xmlbfAttr (name, contents) =
  ( xmlbfName name,
    foldMap (\c -> escapeContent c <> ";") contents
  )

xmlbfName :: XML.Name -> Text
xmlbfName XML.Name {..} = fromMaybe "" namePrefix <> nameLocalName

escapeContent :: XML.Content -> Text
escapeContent = \case
  XML.ContentText t -> t
  XML.ContentEntity e -> HTMLEntities.text e

instance ToXml XML.Node where
  toXml = \case
    XML.NodeElement e -> toXml e
    XML.NodeInstruction i -> mempty
    XML.NodeContent c -> Xmlbf.text $ Text.Lazy.fromStrict $ escapeContent c
    XML.NodeComment t -> mempty

-------------
-- FromXml
-------------

-- | FromXml is not provided for @Document@ because Xmlbf does not net
--   support Processing Instructions or Doctypes.
instance FromXml XML.Element where
  fromXml =
    Xmlbf.pAnyElement $
      XML.Element <$> (name <$> Xmlbf.pName)
        <*> (fmap attr . HashMap.toList <$> Xmlbf.pAttrs)
        <*> (fmap node <$> Xmlbf.pChildren)

attr :: (Text, Text) -> (XML.Name, [XML.Content])
attr (k, v) = (name k, [XML.ContentText v])

name :: Text -> XML.Name
name t = case Text.splitOn ":" t of
  [noPrefix] -> XML.Name
    { XML.nameLocalName = noPrefix,
      XML.nameNamespace = Nothing,
      XML.namePrefix = Nothing
    }
  prefix : rest -> XML.Name
    { XML.nameLocalName = fold rest,
      XML.nameNamespace = Nothing,
      XML.namePrefix = Just prefix
    }
  _ -> XML.Name t Nothing Nothing

node :: Xmlbf.Node -> XML.Node
node = \case
  Xmlbf.Element name' attrs' children' ->
    XML.NodeElement $ XML.Element
      { XML.elementName = name name',
        XML.elementAttributes = attr <$> HashMap.toList attrs',
        XML.elementNodes = node <$> children'
      }
  Xmlbf.Text content ->
    XML.NodeContent $ XML.ContentText $ Text.Lazy.toStrict content
