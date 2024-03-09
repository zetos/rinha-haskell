{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}


import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (..), decode,
                                                       object, (.=))
import           Data.Pool                            (Pool, PoolConfig,
                                                       createPool,
                                                       defaultPoolConfig,
                                                       newPool, withResource)
import           Data.Text.Lazy                       (Text, length, pack)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ     (sql)
import           Database.PostgreSQL.Simple.Time

import           Control.Exception                    (SomeException, try)
import           Control.Monad
import           Data.Time.Clock                      (UTCTime)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       formatTime)
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
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

data BalanceAndTransactions = BalanceAndTransactions {
  bal          :: Int,
  lim          :: Int,
  current_time :: Text,
  transactions :: [Transaction]
}
  deriving (Show, Generic)

instance FromRow BalanceAndTransactions where
  fromRow = BalanceAndTransactions
    <$> field
    <*> field
    <*> (pack <$> (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" :: UTCTime -> String) <$> field)
    <*> (fmap (:[]) $ liftM4 Transaction field field field field)

-- instance FromRow BalanceAndTransactions where
--   fromRow = BalanceAndTransactions <$> field <*> field <*> field <*> ((:[]) <$> liftM4 Transaction field field field field)

instance FromJSON BalanceAndTransactions

instance ToJSON BalanceAndTransactions

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

getBalance :: Connection -> Int -> IO [BalanceAndTransactions]
getBalance conn clientId = query conn [sql|
          WITH latest_transactions AS (
            SELECT cid, amount, type, c_at, descr
            FROM transaction
            WHERE cid = ?
            ORDER BY c_at DESC
            LIMIT 10
          )
          SELECT c.bal, c.lim, NOW() AS current_time,
                 json_agg(json_build_object(
                   'valor', lt.amount,
                   'tipo', lt.type,
                   'realizada_em', lt.c_at,
                   'descricao', lt.descr
                 )) AS transactions
          FROM client c
          LEFT JOIN latest_transactions lt ON c.id = lt.cid
          WHERE c.id = ?
          GROUP BY c.bal, c.lim
        |] (clientId, clientId)

-- validation

validateTransaction :: Transaction -> Either Text Transaction
validateTransaction transaction
  | notElem (tipo transaction) ['c', 'd'] = Left "tipo must be either 'c' or 'd'"
  | Data.Text.Lazy.length (descricao transaction) > 10 = Left "descricao cannot exceed 10 characters"
  | otherwise = Right transaction

main :: IO ()
main = do
  let connectInfo = defaultConnectInfo
        { connectHost = "db"
        , connectPort = 5432
        , connectUser = "admin"
        , connectPassword = "123"
        , connectDatabase = "rinha"
        }

  pool <- createConnectionPool connectInfo

  scotty 8080 $ do
    middleware logStdoutDev -- Add this line to enable request logging

    get "/clientes/:id/extrato" $ do
      -- In a real scenario, you would retrieve the clientId from the request parameters
      let clientId = 2 :: Int
      result <- liftIO $ try $ withResource pool $ \conn ->
        getBalance conn clientId

      case result of
        Left err -> liftIO $ putStrLn $ "Error executing query: " ++ show (err :: SomeException)
        -- Right transactions -> json $ parseTransactions transactions
        Right transactions -> json $ object [ "name" .= String "name", "age" .= Number 123]

    post "/clientes/:id/transacoes" $ do
      bodyBytes <- body
      case (decode bodyBytes :: Maybe Transaction) of
        Just transaction ->
          case validateTransaction transaction of
            Right validatedTransaction -> do
              -- insertStuff pool
              json validatedTransaction
            Left errorText -> do
              status status400
              text errorText
        Nothing -> do
          status status400
          text "Failed to parse JSON"
