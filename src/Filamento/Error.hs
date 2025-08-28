module Filamento.Error where

import Control.Exception (try)
import Control.Monad.Except (MonadError (..))
import Relude

data ErrCode
  = Err0
  | Err1
  | Err2
  | Err3
  | Err4
  | Err5
  | Err6
  | Err7
  | XErr8
  | XErr9
  | XErr10
  | XErr11
  | XErr12
  | XErr13
  | XErr14
  | XErr15
  | XErr16
  | XErr17
  | XErr18
  deriving (Show, Eq)

data Err = Err
  { errCode :: ErrCode,
    errMessage :: Text
  }
  deriving (Show, Eq)

handleExc :: ErrCode -> (SomeException -> Text) -> IO a -> ExceptT Err IO a
handleExc errCode mkErr act = do
  ret <- liftIO $ try act
  case ret of
    Left (e :: SomeException) -> throwError $ Err errCode (mkErr e)
    Right a -> pure a

handleEither :: ErrCode -> (e -> Text) -> Either e a -> ExceptT Err IO a
handleEither errCode mkErr = either (throwError . Err errCode . mkErr) pure

throwErr :: ErrCode -> Text -> ExceptT Err IO ()
throwErr errCode msg = throwError $ Err errCode msg
