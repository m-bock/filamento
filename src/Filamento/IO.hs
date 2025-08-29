module Filamento.IO (parseEnvVars, mkOcto, mkFileAppender) where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Env
import qualified Filamento.Octo as Octo
import Filamento.TypeOps
import Marlin.Core (GCodeLine)
import Network.HTTP.Client
import Network.URI (URI, parseURI)
import Octo.API (OctoHttpCfg (..))
import Relude
import System.Directory (doesFileExist)
import System.Environment (setEnv)

data PrintReport = PrintReport
  { gcodeFile :: FilePath,
    colors :: [(Text, Delta)]
  }
  deriving (Show, Eq, Generic)

instance ToJSON PrintReport

instance FromJSON PrintReport

data PersistentState = PersistentState
  {count :: Int}
  deriving (Show, Eq, Generic)

instance FromJSON PersistentState

instance ToJSON PersistentState

readPersistentState :: IO PersistentState
readPersistentState = do
  let persistentFile = "persistent-state.json"
  c <- readFileBS persistentFile
  v <- case decodeStrict c of
    Just x -> pure x
    Nothing -> error "Failed to decode printing-state.json"

  let v' = v {count = v.count + 1}
  encodeFile persistentFile v'

  pure v

mkFileAppender :: FilePath -> IO (Text -> [GCodeLine] -> IO ())
mkFileAppender filePath = do
  writeFileText filePath ""
  pure $ \tag gLines -> appendFileText filePath $ toText gLines

mkUserInput :: IO (Text -> [GCodeLine] -> IO ())
mkUserInput = do
  pure $ \_ _ -> do
    userInput <- getLine
    let res = parseUserInput userInput
    putTextLn (show res)

mkOcto :: EnvVars -> IO (Text -> [GCodeLine] -> IO ())
mkOcto envVars = do
  manager <- newManager defaultManagerSettings

  let httpConfig =
        OctoHttpCfg
          { manager,
            apiKey = envVars.octoApiKey,
            baseUrl = envVars.octoUrl
          }

  pure $ \tag gLines -> do
    unless envVars.dryRun do
      Octo.sendGCode httpConfig $ toText gLines

---------------------------------------------------------------------------------------------------

data EnvVars = EnvVars
  { dryRun :: Bool,
    octoApiKey :: Text,
    octoUrl :: URI
  }

parseEnvVars :: IO EnvVars
parseEnvVars = do
  loadDotenv

  Env.parse (Env.header "envparse example")
    $ EnvVars
    <$> switch
      "DRY"
      (help "Dry run")
    <*> var
      (str <=< nonempty)
      "OCTO_API_KEY"
      (help "OctoPrint API key")
    <*> var
      envReadUri
      "OCTO_URL"
      (help "OctoPrint URL")

envReadUri :: (AsUnread e) => Env.Reader e URI
envReadUri = Env.eitherReader \str -> maybe (Left "Invalid URI") Right $ parseURI str

---------------------------------------------------------------------------------------------------

data UserInput = UserInput
  { flowCorrection :: Maybe Double
  }
  deriving (Show)

-- e.g. "c.7", "c-9.15", "c-.15"

data UserInputRaw = UserInputRaw
  { cmds :: Map Char ArgVal
  }
  deriving (Show)

data ArgVal = ArgValDouble Double
  deriving (Show)

userInputParseArgVal :: Text -> Either Text ArgVal
userInputParseArgVal str = case parseDoubleWithShorthand str of
  Just d -> Right $ ArgValDouble d
  Nothing -> Left $ "Invalid double: " <> str

userInputParseArg :: Text -> Either Text (Char, ArgVal)
userInputParseArg str = case toString str of
  'c' : rest -> do
    argVal <- userInputParseArgVal (toText rest)
    pure ('c', argVal)
  _ -> Left $ "Invalid argument: " <> str

userInputParseRaw :: Text -> Either Text UserInputRaw
userInputParseRaw str = do
  let parts = T.splitOn " " str & filter (not . T.null) :: [Text]
  ret <- forM parts $ \part -> do
    case userInputParseArg part of
      Right (c, argVal) -> pure (c, argVal)
      Left err -> Left err
  pure $ UserInputRaw {cmds = Map.fromList ret}

userInputFromRaw :: UserInputRaw -> Either Text UserInput
userInputFromRaw (UserInputRaw {cmds}) = do
  flowCorrection <- case Map.lookup 'c' cmds of
    Nothing -> Right Nothing
    Just (ArgValDouble d) -> Right (Just d)
    _ -> Left "flow correction is not a double"
  pure $ UserInput {flowCorrection = flowCorrection}

-- Accepts ".23" -> 0.23 and "-.13" -> -0.13 as well.
parseDoubleWithShorthand :: Text -> Maybe Double
parseDoubleWithShorthand =
  readMaybe . normalize . toString
  where
    normalize ('.' : xs) = '0' : '.' : xs
    normalize ('-' : '.' : xs) = '-' : '0' : '.' : xs
    normalize s = s

parseUserInput :: Text -> Either Text UserInput
parseUserInput str = do
  raw <- userInputParseRaw str
  userInputFromRaw raw

---------------------------------------------------------------------------------------------------

-- Simple .env file loader
loadDotenv :: IO ()
loadDotenv = do
  let envFile = ".env"
  exists <- doesFileExist envFile
  when exists $ do
    content <- readFileText envFile
    forM_ (lines content) $ \line -> do
      let trimmed = T.strip line
      unless (T.null trimmed || T.isPrefixOf "#" trimmed) $ do
        case T.breakOn "=" trimmed of
          (key, value) | not (T.null key) && not (T.null value) -> do
            let key' = T.strip key
                value' = T.strip $ T.drop 1 value -- Remove the "="
            setEnv (toString key') (toString value')
          _ -> pure ()
