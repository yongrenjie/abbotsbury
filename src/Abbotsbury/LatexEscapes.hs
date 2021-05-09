{-# LANGUAGE QuasiQuotes #-}

module Abbotsbury.LatexEscapes where


import           Data.Char                      ( isAscii )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.RawString.QQ


latexEscapes :: Map Char Text
latexEscapes = M.fromList
  [ ('\x00c0', [r|{\`A}|]), ('\x00c1', [r|{\'A}|]), ('\x00c2', [r|{\^A}|]), ('\x00c3', [r|{\~A}|])
  , ('\x00c4', [r|{\"A}|]), ('\x00c5', [r|{\AA}|]), ('\x00c6', [r|{\AE}|]), ('\x00c7', [r|{\cC}|])
  , ('\x00c8', [r|{\`E}|]), ('\x00c9', [r|{\'E}|]), ('\x00ca', [r|{\^E}|]), ('\x00cb', [r|{\"E}|])
  , ('\x00cc', [r|{\`I}|]), ('\x00cd', [r|{\'I}|]), ('\x00ce', [r|{\^I}|]), ('\x00cf', [r|{\"I}|])
  , ('\x00d0', [r|{\DH}|]), ('\x00d1', [r|{\~N}|]), ('\x00d2', [r|{\`O}|]), ('\x00d3', [r|{\'O}|])
  , ('\x00d4', [r|{\^O}|]), ('\x00d5', [r|{\~O}|]), ('\x00d6', [r|{\"O}|]), ('\x00d7', [r|\(\times\)|])
  , ('\x00d8', [r|{\O}|] ), ('\x00d9', [r|{\`U}|]), ('\x00da', [r|{\'U}|]), ('\x00db', [r|{\^U}|])
  , ('\x00dc', [r|{\"U}|]), ('\x00dd', [r|{\'Y}|]), ('\x00de', [r|{\TH}|]), ('\x00df', [r|{\ss}|])

  , ('\x00e0', [r|{\`a}|]), ('\x00e1', [r|{\'a}|]), ('\x00e2', [r|{\^a}|]), ('\x00e3', [r|{\~a}|])
  , ('\x00e4', [r|{\"a}|]), ('\x00e5', [r|{\aa}|]), ('\x00e6', [r|{\ae}|]), ('\x00e7', [r|{\cc}|])
  , ('\x00e8', [r|{\`e}|]), ('\x00e9', [r|{\'e}|]), ('\x00ea', [r|{\^e}|]), ('\x00eb', [r|{\"e}|])
  , ('\x00ec', [r|{\`i}|]), ('\x00ed', [r|{\'i}|]), ('\x00ee', [r|{\^i}|]), ('\x00ef', [r|{\"i}|])
  , ('\x00f0', [r|{\dh}|]), ('\x00f1', [r|{\~n}|]), ('\x00f2', [r|{\`o}|]), ('\x00f3', [r|{\'o}|])
  , ('\x00f4', [r|{\^o}|]), ('\x00f5', [r|{\~o}|]), ('\x00f6', [r|{\"o}|]), ('\x00f7', [r|\(\div\)|])
  , ('\x00f8', [r|{\o}|] ), ('\x00f9', [r|{\`u}|]), ('\x00fa', [r|{\'u}|]), ('\x00fb', [r|{\^u}|])
  , ('\x00fc', [r|{\"u}|]), ('\x00fd', [r|{\'y}|]), ('\x00fe', [r|{\th}|]), ('\x00ff', [r|{\"y}|])

  , ('\x0100', [r|{\=A}|]  ), ('\x0101', [r|{\=a}|]  ), ('\x0102', [r|{\u{A}}|]), ('\x0103', [r|{\u{a}}|])
  , ('\x0104', [r|{\k{A}}|]), ('\x0105', [r|{\k{a}}|]), ('\x0106', [r|{\'C}|]  ), ('\x0107', [r|{\'c}|]  )
  , ('\x0108', [r|{\^C}|]  ), ('\x0109', [r|{\^c}|]  ), ('\x010a', [r|{\.C}|]  ), ('\x010b', [r|{\.c}|]  )
  , ('\x010c', [r|{\v{C}}|]), ('\x010d', [r|{\v{c}}|]), ('\x010e', [r|{\v{D}}|]), ('\x010f', [r|{\v{d}}|])
  , ('\x0110', [r|{\DJ}|]  ), ('\x0111', [r|{\dj}|]  ), ('\x0112', [r|{\=E}|]  ), ('\x0113', [r|{\=e}|]  )
  , ('\x0114', [r|{\u{E}}|]), ('\x0115', [r|{\u{e}}|]), ('\x0116', [r|{\.E}|]  ), ('\x0117', [r|{\.e}|]  )
  , ('\x0118', [r|{\k{E}}|]), ('\x0119', [r|{\k{e}}|]), ('\x011a', [r|{\v{E}}|]), ('\x011b', [r|{\v{e}}|])
  , ('\x011c', [r|{\^G}|]  ), ('\x011d', [r|{\^g}|]  ), ('\x011e', [r|{\u{G}}|]), ('\x011f', [r|{\u{g}}|])

  -- There are some missing ones from this set.
  -- U+0126, U+0127, U+0138, and U+013F all need other packages
  , ('\x0120', [r|{\.G}|]  ), ('\x0121', [r|{\.g}|]   ), ('\x0122', [r|{\c{G}}|]), ('\x0123', [r|{\c{g}}|] )
  , ('\x0124', [r|{\^H}|]  ), ('\x0125', [r|{\^h}|]   )
  , ('\x0128', [r|{\~I}|]  ), ('\x0129', [r|{\~{\i}}|]), ('\x012a', [r|{\=I}|]  ), ('\x012b', [r|{\={\i}}|])
  , ('\x012c', [r|{\u{I}}|]), ('\x012d', [r|{\u{\i}}|]), ('\x012e', [r|{\k{I}}|]), ('\x012f', [r|{\k{i}}|] )
  , ('\x0130', [r|{\.I}|]  ), ('\x0131', [r|{\i}|]    ), ('\x0132', [r|{IJ}|]   ), ('\x0133', [r|{ij}|]    )
  , ('\x0134', [r|{\^J}|]  ), ('\x0135', [r|{\^{\j}}|]), ('\x0136', [r|{\c{K}}|]), ('\x0137', [r|{\c{k}}|] )
  ,                           ('\x0139', [r|{\'L}|]   ), ('\x013a', [r|{\'l}|]  ), ('\x013b', [r|{\c{L}}|] )
  , ('\x013c', [r|{\c{l}}|]), ('\x013d', [r|{\v{L}}|] ), ('\x013e', [r|{\v{l}}|])

  , ('\x0141', [r|{\L}|])
  , ('\x0142', [r|{\l}|])
  , ('\x0143', [r|{\'N}|])
  , ('\x0144', [r|{\'n}|])
  , ('\x2010', "-")
  , ('\x2013', "--")
  , ('\x2014', "---")
  ]


-- | Replace all Unicode special characters with their LaTeX counterparts. Gracefully fail
-- if the replacement cannot be found in the latexEscapes dictionary above.
latexify :: Text -> Text
latexify = replaceEscapes . replaceSpaces
  where
    replaceSpaces :: Text -> Text
    replaceSpaces = T.replace ". " ".\\ "
    replaceEscapes :: Text -> Text
    replaceEscapes = T.concatMap (\c -> if isAscii c
                                           then T.singleton c
                                           else case M.lookup c latexEscapes of
                                                     Just escapeSeq -> escapeSeq
                                                     Nothing        -> T.singleton c
                                 )
