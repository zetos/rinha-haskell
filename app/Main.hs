{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson         (FromJSON, ToJSON, decode)
import           Data.Text.Lazy     (Text, length, pack)
-- import qualified Data.Text.Lazy     as TL

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
  createPool (connect connectInfo) close 1 10 10

validateTransaction :: Transaction -> Either Text Transaction
validateTransaction transaction
  | notElem (tipo transaction) ['c', 'd'] = Left "tipo must be either 'c' or 'd'"
  | Data.Text.Lazy.length (descricao transaction) > 10 = Left "descricao cannot exceed 10 characters"
  | otherwise = Right transaction

main :: IO ()
main = scotty 3000 $ do
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
