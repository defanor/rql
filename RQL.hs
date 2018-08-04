{- |
Module      :  RQL
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  experimental

RQL stands for "restrictable query language", a simplified SQL-like
relational data querying language that can be processed—including
application of restrictions—rather easily. The motivation is to get an
easy way to expose relational databases to untrusted clients that may
need many different and complex queries, without relying on DBMS-level
restrictions alone.

This library aims the PostgreSQL SQL dialect and the
<https://hackage.haskell.org/package/postgresql-simple postgresql-simple>
library at the moment.

See "RQL.Parsing" for the grammar.

== Example

> {-# LANGUAGE OverloadedStrings #-}
> import Database.PostgreSQL.Simple
> import Data.String
> import Data.Aeson
> import qualified Data.ByteString.Char8 as BS
> import qualified Data.ByteString.Lazy.Char8 as BL
> import RQL
> 
> type Organization = Int
> 
> example :: Organization -> BS.ByteString -> BS.ByteString -> IO ()
> example oid dbcString queryString = do
>   conn <- connectPostgreSQL dbcString
>   case processQuery myAdjustments queryString of
>     Left err -> BS.putStrLn err
>     Right (template, params) -> do
>       BS.putStrLn template
>       print params
>       let template' = fromString $
>             "select row_to_json(r) from (" ++ BS.unpack template ++ ") r"
>       BS.putStrLn =<< formatQuery conn template' params
>       r <- map fromOnly <$> query conn template' params
>       BL.putStrLn $ encode (r :: [Value])
>   where
>     myAdjustments :: ExpressionAdjustments
>     myAdjustments = defaultExpressionAdjustments { eaTableCondition = tc }
>       where
>         -- only access current user's organization
>         tc "organizations" = Just $
>           BinOp (Column "organizations" "id") "=" (Literal (BS.pack $ show oid))
>         -- this is redundant, but demonstrates recursive validation of
>         -- the rules
>         tc "users" = Just $ BinOp (Column "users" "organization_id")
>           "in"
>           (ExpressionSelect $
>             Select False [SelectItem (Column "organizations" "id") Nothing]
>             (Just "organizations") [] Nothing [] [] Nothing Nothing)
>         -- prohibit access to the rest
>         tc _ = Just $ Literal "false"
> 


[@Example input query@] @select users.id from users@

[@Resulting query template@] @select ?.? from ? where ((?.? in (select ?.? from ? where ((?.? = ?) and ?))) and ?)@

[@Resulting query@] @select "users"."id" from "users" where (("users"."organization_id" in (select "organizations"."id" from "organizations" where (("organizations"."id" = '42') and 'true'))) and 'true')@

== Usage tips

* The AST types come with 'Generic' instances, so an easy to implement
  alternative to query parsing is to deserialize those queries from
  JSON, for instance.
* While this library should be pretty safe, it is usually a good idea
  to use separate DBMS users for different clients and\/or tasks,
  restricting those on the DBMS level as much as practical (e.g., with
  <https://www.postgresql.org/docs/current/static/ddl-rowsecurity.html Row Security Policies>).
* In order to reject potentially costly queries, estimate query costs
  with 'queryCost' before executing them.

-}

{-# LANGUAGE OverloadedStrings #-}
module RQL (
  -- * Query processing
  processQuery, buildQuery, getParams
  -- * Settings
  , ExpressionAdjustments (..), defaultExpressionAdjustments
  -- * Types
  , module RQL.Types
  -- * Utility functions
  , module RQL.Util
  ) where

import RQL.Types
import RQL.Parsing
import RQL.Validation
import RQL.Util

import Database.PostgreSQL.Simple.ToField (Action)
import Data.Monoid ((<>))
import Data.Attoparsec.ByteString.Char8
import Control.Arrow

import qualified Data.ByteString.Char8 as BS

-- | Builds a query template out of validated chunks.
buildQuery :: [Validated] -> BS.ByteString
buildQuery [] = BS.empty
buildQuery (VQuery x : xs) = x <> buildQuery xs
buildQuery (_ : xs) = "?" <> buildQuery xs

-- | Extracts query parameters out of validated chunks.
getParams  :: [Validated] -> [Action]
getParams [] = []
getParams (VParam x : xs) = x : getParams xs
getParams (_ : xs) = getParams xs

-- | Reads a query, produces a query template along with a list of
-- parameters.
processQuery :: ExpressionAdjustments
             -> BS.ByteString
             -> Either BS.ByteString (BS.ByteString, [Action])
processQuery ea q = case parseOnly (select <* endOfInput) q of
  Left err -> Left $ BS.pack $ "Query parsing failure: " ++ err
  Right r -> (buildQuery &&& getParams) <$> runValidator (vSelect ea r)
