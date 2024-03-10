{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Db (createConnectionPool, getBalance, transactionUpdateBalance, getConnectionPool, BalanceAndTransactions(..)) where

import           Data.Aeson                         (FromJSON, ToJSON,
                                                     Value (..))
import           Data.Pool                          (Pool, defaultPoolConfig,
                                                     newPool)
import           Data.Text.Lazy                     (Text, pack)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ   (sql)

import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Format                   (defaultTimeLocale,
                                                     formatTime)
import           GHC.Generics

-- instance FromRow BalanceAndTransactions where
--   fromRow = BalanceAndTransactions
--     <$> field
--     <*> field
--     <*> (pack <$> (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" :: UTCTime -> String) <$> field)
--     <*> (toJSON <$> liftM4 Transaction field field field field)

data BalanceAndTransactions = BalanceAndTransactions {
  total              :: Int,
  limite             :: Int,
  data_extrato       :: Text,
  ultimas_transacoes :: Value
}
  deriving (Show, Generic)

instance FromRow BalanceAndTransactions where
  fromRow = BalanceAndTransactions
    <$> field
    <*> field
    <*> (pack <$> (formatTime defaultTimeLocale "%FT%T%QZ" :: UTCTime -> String) <$> field)
    <*> field

instance FromJSON BalanceAndTransactions

instance ToJSON BalanceAndTransactions

data UpdateResult = UpdateResult {
  saldo  :: Int,
  limite :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow)

instance FromJSON UpdateResult

instance ToJSON UpdateResult

createConnectionPool :: ConnectInfo -> IO (Pool Connection)
createConnectionPool connectInfo = do
  let poolConfig = defaultPoolConfig (connect connectInfo) close 60 10
  newPool poolConfig

getConnectionPool :: IO (Pool Connection)
getConnectionPool = do
  let connectInfo = defaultConnectInfo
        { connectHost = "db"
        , connectPort = 5432
        , connectUser = "admin"
        , connectPassword = "123"
        , connectDatabase = "rinha"
        }
  createConnectionPool connectInfo

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

transactionUpdateBalance :: Connection -> Int -> String -> Int -> Text -> IO [UpdateResult]
transactionUpdateBalance conn clientId typeT amount description = query conn [sql|
    WITH inserted_transaction AS (
      INSERT INTO transaction (cid, amount, type, descr)
      VALUES (?, ?, ?, ?)
      RETURNING *
    )
    UPDATE client
    SET bal = bal + CASE WHEN ? = 'd' THEN -? ELSE ? END
    WHERE id = ?
    RETURNING bal, lim
  |] (clientId, amount, typeT, description, typeT, amount, amount, clientId)
