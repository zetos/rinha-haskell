{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson                 (FromJSON, ToJSON, decode)
import           Data.Pool                  (Pool, PoolConfig, createPool,
                                             defaultPoolConfig, newPool,
                                             withResource)
import           Data.Text.Lazy             (Text, length, pack)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.HTTP.Types
import           Web.Scotty


data Transaction = Transaction
  { valor        :: Int,
    tipo         :: Char,
    descricao    :: Text,
    realizada_em :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Transaction

instance ToJSON Transaction

data Saldo = Saldo
  { total        :: Int,
    data_extrato :: Text,
    limite       :: Int
  }
  deriving (Show, Generic)

instance FromJSON Saldo

instance ToJSON Saldo

data AccountInfo = AccountInfo
  { saldo              :: Saldo,
    ultimas_transacoes :: [Transaction]
  }
  deriving (Show, Generic)

instance FromJSON AccountInfo

instance ToJSON AccountInfo

-- DB


createConnectionPool :: ConnectInfo -> IO (Pool Connection)
createConnectionPool connectInfo = do
  let poolConfig = defaultPoolConfig (connect connectInfo) close 60 10
  newPool poolConfig

validateTransaction :: Transaction -> Either Text Transaction
validateTransaction transaction
  | notElem (tipo transaction) ['c', 'd'] = Left "tipo must be either 'c' or 'd'"
  | Data.Text.Lazy.length (descricao transaction) > 10 = Left "descricao cannot exceed 10 characters"
  | otherwise = Right transaction

main :: IO ()
main = do
  let connectInfo = defaultConnectInfo
        { connectHost = "localhost"
        , connectPort = 5432
        , connectUser = "admin"
        , connectPassword = "123"
        , connectDatabase = "rinha"
        }

  pool <- createConnectionPool connectInfo

  scotty 8080 $ do
    get "/clientes/:id/extrato" $ do
      -- clientId <- pathParam "id"
      -- json $ object ["message" .= String ("Hello, " <> clientId <> "!")]
      json $ AccountInfo {
        saldo = Saldo { total = 666, data_extrato = "foo", limite = 9000 },
        ultimas_transacoes = [
          Transaction {
             valor = 10,
             tipo = 'd',
             descricao = "descrip..",
             realizada_em = Just "foobar"
              }
          ]
        }

    post "/clientes/:id/transacoes" $ do
      bodyBytes <- body
      case (decode bodyBytes :: Maybe Transaction) of
        Just transaction ->
          case validateTransaction transaction of
            Right validatedTransaction -> json validatedTransaction
            Left errorText -> do
              status status400
              text errorText
        Nothing -> do
          status status400
          text "Failed to parse JSON"
