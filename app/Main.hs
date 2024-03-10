{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}


import           Control.Exception          (SomeException, try)
import           Data.Aeson                 (FromJSON, ToJSON, decode)
import           Data.Pool                  (withResource)
import           Data.Text.Lazy             (Text, length)
import           Database.PostgreSQL.Simple
import qualified Db                         (getBalance, getConnectionPool,
                                             transactionUpdateBalance)
-- import           Debug.Trace                          (traceShowId)
import           GHC.Generics
import           Network.HTTP.Types
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty

data Transaction = Transaction
  { valor        :: Int,
    tipo         :: Char,
    realizada_em :: Maybe Text,
    descricao    :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow)

instance FromJSON Transaction

instance ToJSON Transaction

validateTransaction :: Transaction -> Either Text Transaction
validateTransaction transaction
  | notElem (tipo transaction) ['c', 'd'] = Left "tipo must be either 'c' or 'd'"
  | Data.Text.Lazy.length (descricao transaction) > 10 = Left "descricao cannot exceed 10 characters"
  | otherwise = Right transaction

main :: IO ()
main = do
  pool <- Db.getConnectionPool

  scotty 8080 $ do
    -- middleware logStdoutDev -- Add this line to enable request logging

    get "/clientes/:id/extrato" $ do
      clientId <- pathParam "id"
      result <- liftIO $ try $ withResource pool $ \conn ->
        Db.getBalance conn clientId

      case result of
        Left err -> liftIO $ putStrLn $ "Error executing query: " ++ show (err :: SomeException)
        Right extract -> do
          if null extract
            then status status404
            else json (head extract)

    post "/clientes/:id/transacoes" $ do
      clientId <- pathParam "id"
      bodyBytes <- body
      case (decode bodyBytes :: Maybe Transaction) of
        Just transaction ->
          case validateTransaction transaction of
            Right validatedTransaction -> do
              result <- liftIO $ try $ withResource pool $ \conn ->
                      Db.transactionUpdateBalance conn clientId [(tipo validatedTransaction)] (valor validatedTransaction) (descricao validatedTransaction)

              case result of
                Left (_ :: SomeException) -> status status422
                Right transactions -> do
                  if null transactions
                    then status status404
                    else json (head transactions)
            Left errorText -> do
              status status400
              text errorText
        Nothing -> do
          status status400
          text "Failed to parse JSON"
