module GL.Args
  ( module GL.Args
  )
where

import           Options.Applicative

data Args =
  Args
    { inputFileArg :: FilePath
    , outputFileArg :: Maybe FilePath
    }
  deriving (Show)

argsParser :: Parser Args
argsParser =
  Args <$> strArgument (metavar "INPUT" <> help "Input file path") <*> optional
    (strOption
      (metavar "OUTPUT" <> long "output" <> short 'o' <> help "Output file path"
      )
    )

getArgs :: IO Args
getArgs = execParser $ info
  (argsParser <**> helper)
  (fullDesc <> progDesc "Compile a GregLang program" <> header
    "GregLangCompiler - GregLang compiler"
  )
