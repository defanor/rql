{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  RQL.Util
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  experimental

Utility functions.
-}

module RQL.Util (queryCost) where

import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Monoid

data PGPlan = PGPlan { pgpTotalCost :: Double }
  deriving (Show, Eq)

instance FromJSON PGPlan where
  parseJSON (Object v) = do
    plan <- v .: "Plan"
    PGPlan <$> (plan .: "Total Cost")
  parseJSON _ = fail "PGPlan must be an object"

-- | Estimates query cost with @explain@.
queryCost :: ToRow q => Connection -> Query -> q -> IO (Either String Double)
queryCost c q p = do
  let costQuery = "explain (format json) " <> q
  r <- query c costQuery p
  pure $ case r of
    [Only cost] -> case fromJSON cost :: Result [PGPlan] of
      Error err -> Left $ "Failed to parse query plan: " ++ err
      Success plans -> Right $ sum (map pgpTotalCost plans)
    _ -> Left "Unexpected cost query result"
