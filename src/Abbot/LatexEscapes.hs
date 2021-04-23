{-# LANGUAGE QuasiQuotes #-}

module Abbot.LatexEscapes where


import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.RawString.QQ


latexEscapes :: Map Text Text
latexEscapes = M.fromList
  [ ("\x00c0", [r|{\`A}|]), ("\x00c1", [r|{\'A}|]), ("\x00c2", [r|{\^A}|]), ("\x00c3", [r|{\~A}|])
  , ("\x00c4", [r|{\"A}|]), ("\x00c5", [r|{\AA}|]), ("\x00c6", [r|{\AE}|]), ("\x00c7", [r|{\cC}|])
  , ("\x00c8", [r|{\`E}|]), ("\x00c9", [r|{\'E}|]), ("\x00ca", [r|{\^E}|]), ("\x00cb", [r|{\"E}|])
  , ("\x00cc", [r|{\`I}|]), ("\x00cd", [r|{\'I}|]), ("\x00ce", [r|{\^I}|]), ("\x00cf", [r|{\"I}|])
  , ("\x00d0", [r|{\DH}|]), ("\x00d1", [r|{\~N}|]), ("\x00d2", [r|{\`O}|]), ("\x00d3", [r|{\'O}|])
  , ("\x00d4", [r|{\^O}|]), ("\x00d5", [r|{\~O}|]), ("\x00d6", [r|{\"O}|]), ("\x00d7", [r|\(\times\)|])
  , ("\x00d8", [r|{\O}|] ), ("\x00d9", [r|{\`U}|]), ("\x00da", [r|{\'U}|]), ("\x00db", [r|{\^U}|])
  , ("\x00dc", [r|{\"U}|]), ("\x00dd", [r|{\'Y}|]), ("\x00de", [r|{\TH}|]), ("\x00df", [r|{\ss}|])

  , ("\x00e0", [r|{\`a}|]), ("\x00e1", [r|{\'a}|]), ("\x00e2", [r|{\^a}|]), ("\x00e3", [r|{\~a}|])
  , ("\x00e4", [r|{\"a}|]), ("\x00e5", [r|{\aa}|]), ("\x00e6", [r|{\ae}|]), ("\x00e7", [r|{\cc}|])
  , ("\x00e8", [r|{\`e}|]), ("\x00e9", [r|{\'e}|]), ("\x00ea", [r|{\^e}|]), ("\x00eb", [r|{\"e}|])
  , ("\x00ec", [r|{\`i}|]), ("\x00ed", [r|{\'i}|]), ("\x00ee", [r|{\^i}|]), ("\x00ef", [r|{\"i}|])
  , ("\x00f0", [r|{\dh}|]), ("\x00f1", [r|{\~n}|]), ("\x00f2", [r|{\`o}|]), ("\x00f3", [r|{\'o}|])
  , ("\x00f4", [r|{\^o}|]), ("\x00f5", [r|{\~o}|]), ("\x00f6", [r|{\"o}|]), ("\x00f7", [r|\(\div\)|])
  , ("\x00f8", [r|{\o}|] ), ("\x00f9", [r|{\`u}|]), ("\x00fa", [r|{\'u}|]), ("\x00fb", [r|{\^u}|])
  , ("\x00fc", [r|{\"u}|]), ("\x00fd", [r|{\'y}|]), ("\x00fe", [r|{\th}|]), ("\x00ff", [r|{\"y}|])

  , ("\x0106", [r|{\'C}|])
  , ("\x0107", [r|{\'c}|])
  , ("\x010d", [r|{\v{c}}|])
  , ("\x0112", [r|{\=E}|])
  , ("\x0141", [r|{\L{}}|])
  , ("\x0142", [r|{\l{}}|])
  , ("\x0143", [r|{\'N}|])
  , ("\x0144", [r|{\'n}|])
  , ("\x2010", "-")
  , ("\x2013", "--")
  , ("\x2014", "---")
  ]


-- | Replace all Unicode special characters with their LaTeX counterparts.
latexify :: Text -> Text
latexify input = replaceSpaces $ M.foldlWithKey replaceEscapes input latexEscapes
  where
    replaceSpaces :: Text -> Text
    replaceSpaces = T.replace ". " ".\\ "
    replaceEscapes :: Text -> Text -> Text -> Text
    replaceEscapes before key val = T.replace key val before
