module Filamento.IO
  ( parseEnvVars,
    mkHookFileAppender,
    mkHookUserInput,
    mkHookOcto,
    mkHookLogger,
    mkHookFiles,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Env
import Filamento.Core (GCodeHook (..))
import qualified Filamento.Octo as Octo
import Filamento.TypeOps
import Network.HTTP.Client
import Network.URI (URI, parseURI)
import Octo.API (OctoHttpCfg (..))
import Relude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive)
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

mkHookFileAppender :: FilePath -> IO GCodeHook
mkHookFileAppender filePath = do
  writeFileText filePath ""
  pure
    $ GCodeHook
      \_ gLines -> do
        putTextLn $ "[FileAppender] Appending " <> show (length gLines) <> " lines to " <> cs filePath
        appendFileText filePath $ toText gLines

mkHookFiles :: IO GCodeHook
mkHookFiles = do
  counterRef <- liftIO $ newIORef 0

  let dir = "out/current"
  dirExists <- doesDirectoryExist dir
  when dirExists $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir

  pure $ GCodeHook
    \tag gLines -> do
      counter <- liftIO $ readIORef counterRef
      let filePath = dir <> "/print-" <> show counter <> ".gcode"
      writeFileText filePath $ toText gLines

      liftIO $ writeIORef counterRef (counter + 1)

mkHookUserInput :: EnvVars -> IO GCodeHook
mkHookUserInput envVars = do
  pure $ GCodeHook
    \tag _ -> do
      if envVars.dryRun
        then putTextLn ("[UserInput] " <> tag <> " Dry run")
        else do
          putTextLn ("[UserInput] " <> tag <> " Waiting for input")
          let loop = do
                putStr "> "
                hFlush stdout
                userInput <- getLine
                let res = parseUserInput userInput
                putTextLn (show res)
                case res of
                  Right _ -> pure ()
                  Left err -> do
                    loop

          loop

mkHookOcto :: EnvVars -> IO GCodeHook
mkHookOcto envVars = do
  manager <- newManager defaultManagerSettings

  let httpConfig =
        OctoHttpCfg
          { manager,
            apiKey = envVars.octoApiKey,
            baseUrl = envVars.octoUrl
          }

  pure $ GCodeHook
    \_ gLines -> do
      if envVars.dryRun
        then putTextLn "[Octo] Dry run"
        else do
          putTextLn "[Octo] Sending GCode"
          liftIO $ Octo.sendGCode httpConfig $ toText gLines

mkHookLogger :: IO GCodeHook
mkHookLogger = do
  pure $ GCodeHook
    \tag gLines -> do
      putTextLn $ "[Logger] " <> cs tag

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
