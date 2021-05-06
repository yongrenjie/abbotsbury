-- | Includes the optparse-applicative code which parses the command-line options for the main
-- function.
module Options where


import           Abbotsbury
import           Abbotsbury.Work
import           Abbotsbury.Cite.Internal


import           Data.Char                      ( toLower )
import           Options.Applicative     hiding ( style )
import qualified Text.PrettyPrint.ANSI.Leijen  as PP


-- | Command-line option parsing for the executable itself. At the moment, the
-- only option available is the starting directory, which is essentially argv[1].
newtype AbbotOptions = AbbotOptions
             { optCommand :: AbbotCommand
             }


data AbbotCommand = AbbotMain AbbotMainOptions
                  | AbbotCite AbbotCiteOptions


data AbbotMainOptions = AbbotMainOptions
                      { startingDirectory :: FilePath
                      }


-- | Wraps the command-line Parser into a ParserInfo structure, which contains "top-level"
-- information. This is a required step for actually executing a parser (see optparse README).
parserInfo :: ParserInfo AbbotCommand
parserInfo = info
  (helper <*> parser)
  (fullDesc <> progDesc "Minimalistic command-line reference manager")


parser :: Parser AbbotCommand
parser = mainParser <|> subparsers


subparsers :: Parser AbbotCommand
subparsers = subparser (command "cite" citeParserInfo)


-- | The command-line parser for the interactive abbotsbury REPL.
mainParser :: Parser AbbotCommand
mainParser =
  AbbotMain
    <$> (AbbotMainOptions <$> strOption
          (  short 'd'
          <> long "directory"
          <> help "Directory to start in"
          <> value "."
          <> showDefaultWith (const "current working directory")
          )
        )


data AbbotCiteOptions = AbbotCiteOptions
  { citeDOI :: DOI
  , style :: Style
  , format :: Format
  }


-- | Option reader which converts a string (given on command line) to a concrete style.
styleReader :: ReadM Style
styleReader = eitherReader $ \s ->
  let s' = map toLower s
  in  case s' of
        _
          | s' `elem` ["a", "acs"] -> Right acsStyle
          | s' `elem` ["b", "bib"] -> Right bibStyle
          | otherwise -> Left $ mconcat
            [ "style '"
            , s
            , "' not recognised.\nRun `abbot cite -h` to see acceptable values."
            ]


-- | Option reader which converts a string (given on command line) to a concrete format.
formatReader :: ReadM Format
formatReader = eitherReader $ \f ->
  let f' = map toLower f
  in  case f' of
        _ | f' `elem` ["t", "text"]                -> Right textFormat
          | f' `elem` ["m", "md", "markdown"]      -> Right markdownFormat
          | f' `elem` ["r", "rst", "restructured"] -> Right restructuredFormat
          | f' `elem` ["h", "html"]                -> Right htmlFormat
          | otherwise -> Left $ mconcat
            [ "format '"
            , f
            , "' not recognised.\nRun `abbot cite -h` to see acceptable values."
            ]


citeParser :: Parser AbbotCommand
citeParser =
  AbbotCite
    <$> (   AbbotCiteOptions
        <$> strArgument (metavar "DOI" <> helpDoc (Just doiHelp))
        <*> option styleReader
                   (metavar "STYLE"
                   <> long "style"
                   <> short 's'
                   <> helpDoc (Just styleHelp))
        <*> option formatReader
                   (metavar "FORMAT"
                   <> long "format"
                   <> short 'f'
                   <> value textFormat
                   <> helpDoc (Just formatHelp))
        )
 where
  doiHelp :: PP.Doc
  doiHelp =
    PP.nest 2 $ PP.vsep [PP.text "DOI of paper to generate citation for"]
  styleHelp :: PP.Doc
  styleHelp = PP.nest 2 $ PP.vsep
    [ PP.text "Citation style to use. Acceptable options:"
    , PP.text "{a, acs}               - ACS"
    , PP.text "{b, bib}               - BibLaTeX"
    ]
  formatHelp :: PP.Doc
  formatHelp = PP.nest 2 $ PP.vsep
    [ PP.text "Output format to use. Acceptable options:"
    , PP.text "{t, text}              - Plain text (default)"
    , PP.text "{m, md, markdown}      - Markdown"
    , PP.text "{r, rst, restructured} - reStructuredText"
    , PP.text "{h, html}              - HTML"
    ]


citeParserInfo :: ParserInfo AbbotCommand
citeParserInfo = info (helper <*> citeParser)
  ( fullDesc
  <> progDesc "Generate a citation for one specific paper, using a particular citation style and format."
  <> header "Single-use citation generator"
  )
