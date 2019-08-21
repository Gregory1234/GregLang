module GL.Args
  ( module GL.Args
  )
where

import           Data.Maybe
import qualified System.Console.ParseArgs      as A

data Args =
  Args
    { inputFileArg :: FilePath
    , outputFileArg :: FilePath
    }
  deriving (Show)

schema :: [A.Arg String]
schema =
  [ A.Arg "out"
          (Just 'o')
          (Just "output")
          (A.argDataOptional "output" A.ArgtypeString)
          "Output file"
  , A.Arg "in"
          Nothing
          Nothing
          (A.argDataRequired "input" A.ArgtypeString)
          "Input file"
  ]

getArgs :: IO Args
getArgs = do
  a <- A.parseArgsIO A.ArgsComplete schema
  return $ Args
    (A.getRequiredArg a "in")
    (fromMaybe (A.getRequiredArg a "in" ++ ".o") (A.getArg a "out"))
