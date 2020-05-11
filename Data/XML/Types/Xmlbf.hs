{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.XML.Types.Xmlbf where

import Data.Foldable (foldMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.XML.Types as XML
import qualified HTMLEntities.Text as HTMLEntities
import qualified Xmlbf
import Xmlbf (FromXml (..), ToXml (..))

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
