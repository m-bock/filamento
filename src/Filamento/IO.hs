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
import Filamento.Core
import qualified Filamento.Octo as Octo
import Filamento.TypeOps
import Fmt (fixedF, padLeftF, (+|), (|+))
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

mkHookFileAppender :: FilePath -> IO HookEmitGCode
mkHookFileAppender filePath = do
  writeFileText filePath ""
  pure
    $ HookEmitGCode
      \_ gLines -> do
        putTextLn $ "[FileAppender] Appending " <> show (length gLines) <> " lines to " <> cs filePath
        appendFileText filePath $ toText gLines

mkHookFiles :: IO HookEmitGCode
mkHookFiles = do
  let baseDir = "out/current" :: Text
  dirExists <- doesDirectoryExist (cs baseDir)
  when dirExists $ removeDirectoryRecursive (cs baseDir)
  createDirectoryIfMissing True (cs baseDir)

  getFilePath <- liftIO $ mkGetFilePath baseDir

  pure $ HookEmitGCode
    \tag gLines -> do
      env <- ask

      filePath <- liftIO $ getFilePath tag env.sectionPath

      writeFileText (cs filePath) $ toText gLines

mkGetFilePath :: Text -> IO (Text -> [Text] -> IO Text)
mkGetFilePath baseDir = do
  refCounter <- newIORef 0 :: IO (IORef Int)
  pure \tag sectionPath -> do
    counter <- readIORef refCounter
    writeIORef refCounter (counter + 1)

    let pathPart = intercalate "-" (map toString sectionPath)

    pure $ baseDir |+ "/" +| padLeftF 4 '0' counter |+ "-" +| pathPart |+ ".gcode"

mkHookUserInput :: EnvVars -> IO HookUserInput
mkHookUserInput envVars = do
  pure $ HookUserInput
    \tag -> do
      putTextLn ("[UserInput] " <> tag)
      st <- gcodeStateGet
      let userState = deriveUserState st
      putTextLn (printUserState userState)

      if envVars.dryRun
        then putTextLn ("Dry run")
        else do
          let loop = do
                putStr "> "
                hFlush stdout
                userInput <- getLine
                let res = parseUserInput userInput
                case res of
                  Right ui -> do
                    applyUserInput ui
                    st' <- gcodeStateGet
                    let us = deriveUserState st'
                    comment ("User input: " <> printUserState us)
                  Left err -> do
                    putTextLn err
                    loop

          loop

data UserState = UserState
  { flowCorrection :: Double
  }
  deriving (Show, Eq, Generic)

deriveUserState :: GCodeState -> UserState
deriveUserState st = UserState {flowCorrection = st.flowCorrection}

printUserState :: UserState -> Text
printUserState st = "flowCorrection: [c] " +| fixedF 2 st.flowCorrection |+ ""

mkHookOcto :: EnvVars -> IO HookEmitGCode
mkHookOcto envVars = do
  manager <- newManager defaultManagerSettings

  let httpConfig =
        OctoHttpCfg
          { manager,
            apiKey = envVars.octoApiKey,
            baseUrl = envVars.octoUrl
          }

  pure $ HookEmitGCode
    \_ gLines -> do
      if envVars.dryRun
        then putTextLn "[Octo] Dry run"
        else do
          putTextLn "[Octo] Sending GCode"
          liftIO $ Octo.sendGCode httpConfig $ toText gLines

mkHookLogger :: IO HookEmitGCode
mkHookLogger = do
  pure $ HookEmitGCode
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

applyUserInput :: UserInput -> GCode ()
applyUserInput userInput = do
  case userInput.flowCorrection of
    Nothing -> pure ()
    Just fc -> setFlowCorrection fc

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
