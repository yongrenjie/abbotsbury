-- | Includes the optparse-applicative code which parses the command-line options for the main
-- function.
module Options where

import           Abbotsbury
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import           Data.Char                      ( toLower )
import           Data.Version
import           Options.Applicative     hiding ( style )
import           Paths_abbotsbury
import qualified Text.PrettyPrint.ANSI.Leijen  as PP

-- | Command-line option parsing for the executable itself. At the moment, the
-- only option available is the starting directory, which is essentially argv[1].
newtype AbbotOptions = AbbotOptions
  { optCommand :: AbbotCommand
  }

data AbbotCommand
  = AbbotMain AbbotMainOptions
  | AbbotCite AbbotCiteOptions

data AbbotMainOptions = AbbotMainOptions
  { -- | The working directory to start in.
    startingDirectory :: FilePath
  ,
    -- | Quiet flag.
    verbosity         :: AbbotVerbosity
  , 
    -- | Whether to show the version number and exit.
    mainVersion       :: Bool
  }

data AbbotVerbosity = NormalVerbosity | Quiet
                    deriving (Eq, Show)

abbotParserPrefs :: ParserPrefs
abbotParserPrefs =
  prefs (multiSuffix "..." <> showHelpOnEmpty)

-- | Wraps the command-line Parser into a ParserInfo structure, which contains "top-level"
-- information. This is a required step for actually executing a parser (see optparse README).
parserInfo :: ParserInfo AbbotCommand
parserInfo = info
  (helper <*> parser)
  (fullDesc <> progDesc "Minimalistic command-line reference manager." <> header
    ("abbot v" <> showVersion version)
  )

parser :: Parser AbbotCommand
parser = mainParser <|> subparsers

subparsers :: Parser AbbotCommand
subparsers = subparser (command "cite" citeParserInfo)

-- | The command-line parser for the interactive abbotsbury REPL.
mainParser :: Parser AbbotCommand
mainParser =
  AbbotMain
    <$> (   AbbotMainOptions
        <$> strOption
              (  short 'd'
              <> long "directory"
              <> help "Directory to start in"
              <> value "."
              <> showDefaultWith (const "current working directory")
              )
        <*> flag NormalVerbosity Quiet
              (  short 'q'
              <> long "quiet"
              <> help "Silence prompt"
              )
        <*> switch (long "version" <> help "Display version number and exit")
        )

data AbbotCiteOptions = AbbotCiteOptions
  { -- | The DOIs to cite.
    citeDOI     :: [DOI]
  ,
    -- | The output settings.
    sfc         :: StyleFormatCopy
  ,
    -- | Whether to get the email from `git config --get user.email`.
    useGitEmail :: Bool
  }

data CopyOption = NoCopy | CopyAsText | CopyAsRtf deriving (Eq, Show, Bounded, Enum)

type StyleFormatCopy = (Style, Format, CopyOption)

-- | Option reader which converts a string (given on command line) to a concrete style.
styleReader :: ReadM Style
styleReader = eitherReader $ \s ->
  let s' = map toLower s
  in
    case s' of
      _
        | s' `elem` ["a", "acs"] -> Right acsStyle
        | s' `elem` ["as", "acs-short"] -> Right acsShortStyle
        | s' `elem` ["b", "bib"] -> Right bibStyle
        | otherwise -> Left $ makeInvalidArgMessage s

-- | Option reader which converts a string (given on command line) to a concrete format.
formatReader :: ReadM Format
formatReader = eitherReader $ \f ->
  let f' = map toLower f
  in
    case f' of
      _
        | f' `elem` ["t", "text"] -> Right textFormat
        | f' `elem` ["m", "md", "markdown"] -> Right markdownFormat
        | f' `elem` ["r", "rst", "restructured"] -> Right restructuredFormat
        | f' `elem` ["h", "html"] -> Right htmlFormat
        | otherwise -> Left $ makeInvalidArgMessage f

-- | Option reader which converts a string (given on command line) to a concrete format.
copyReader :: ReadM CopyOption
copyReader = eitherReader $ \c ->
  let c' = map toLower c
  in
    case c' of
      _
        | c' == "none" -> Right NoCopy
        | c' == "text" -> Right CopyAsText
        | c' == "rtf"  -> Right CopyAsRtf
        | otherwise -> Left $ makeInvalidArgMessage c

sfcOption :: Parser StyleFormatCopy
sfcOption = wordSfc <|> manualSfc
 where
  wordSfc, manualSfc :: Parser StyleFormatCopy
  wordSfc = flag'
    (acsShortStyle, textFormat, CopyAsRtf)
    (short 'W' <> help "alias for '-s acs-short -f text -c rtf'")
  manualSfc =
    (,,)
      <$> option
            styleReader
            (  metavar "STYLE"
            <> long "style"
            <> short 's'
            <> value acsShortStyle
            <> helpDoc (Just styleHelp)
            )
      <*> option
            formatReader
            (  metavar "FORMAT"
            <> long "format"
            <> short 'f'
            <> value textFormat
            <> helpDoc (Just formatHelp)
            )
      <*> option
            copyReader
            (  metavar "COPY"
            <> long "copy"
            <> short 'c'
            <> value NoCopy
            <> helpDoc (Just copyHelp)
            )
  styleHelp, formatHelp, copyHelp :: PP.Doc
  styleHelp = PP.nest 2 $ PP.vsep
    [ PP.text "Citation style to use. Acceptable options:"
    , PP.text "{a, acs}               - ACS, with title and DOI"
    , PP.text "{as, acs-short}        - ACS, no title or DOI"
    , PP.text "{b, bib}               - BibLaTeX"
    ]
  formatHelp = PP.nest 2 $ PP.vsep
    [ PP.text "Output format to use. Acceptable options:"
    , PP.text "{t, text}              - Plain text (default)"
    , PP.text "{m, md, markdown}      - Markdown"
    , PP.text "{r, rst, restructured} - reStructuredText"
    , PP.text "{h, html}              - HTML"
    ]
  copyHelp = PP.nest 2 $ PP.vsep
    [ PP.text "Whether to copy text to clipboard. Acceptable options:"
    , PP.text "none                   - don't copy (default)"
    , PP.text "text                   - copy as plain text"
    , PP.text "rtf                    - copy as formatted text (only on macOS)"
    ]

makeInvalidArgMessage :: String -> String
makeInvalidArgMessage badArgVal = mconcat
  [ "argument '"
  , badArgVal
  , "' not recognised.\nRun `abbot cite -h` to see acceptable values."
  ]

citeParser :: Parser AbbotCommand
citeParser =
  AbbotCite
    <$> (   AbbotCiteOptions
        <$> some (strArgument (metavar "DOI" <> helpDoc (Just doiHelp)))
        <*> sfcOption
        <*> switch
              (long "use-git-email" <> short 'G' <> help
                "Use email from `git config` to fetch metadata"
              )
        )
 where
  doiHelp :: PP.Doc
  doiHelp = PP.nest 2 $ PP.vsep [PP.text "DOI(s) to generate citations for."]

citeParserInfo :: ParserInfo AbbotCommand
citeParserInfo = info
  (helper <*> citeParser)
  (  fullDesc
  <> progDesc
       "Generate citations for one or more papers, using a particular citation style and format."
  <> header ("abbot-cite v" <> showVersion version)
  )
