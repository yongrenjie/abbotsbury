module Abbot.Cite.Part where


import           Data.Text                      ( Text )


-- | "Formatted" parts of a citation, which can later be converted to real Text objects based on the
-- CiteFormat used.
data CitationPart = Plain Text
                  | Bold Text
                  | Italic Text
                  | Link Text Text


