# RQL

RQL stands for "restrictable query language", a simplified SQL-like
relational data querying language that can be processed—including
application of restrictions—rather easily. The motivation is to get an
easy way to expose relational databases to untrusted clients that may
need many different and complex queries, without relying on DBMS-level
restrictions alone.


## Grammar

A brief and informal version (see the Haddock documentation for more):

```
select
  [distinct]
  [(<expression> [as <identifier>]) [, ...]]
  [from <identifier>]
  [(inner|left) join <identifier> on <expression>]
  [where <expression>]
  [group by <expression> [, ...]]
  [order by <expression> [asc|desc] [, ...]]
  [limit <number>]
  [offset <number>]
```

Columns should be accompanied by table names, binary operator
applications should be parenthesized.


## Example

```haskell
import Database.PostgreSQL.Simple
import Data.String
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import RQL

type Organization = Int

example :: Organization -> BS.ByteString -> BS.ByteString -> IO ()
example oid dbcString queryString = do
  conn <- connectPostgreSQL dbcString
  case processQuery myAdjustments queryString of
    Left err -> BS.putStrLn err
    Right (template, params) -> do
      BS.putStrLn template
      print params
      let template' = fromString $
            "select row_to_json(r) from (" ++ BS.unpack template ++ ") r"
      BS.putStrLn =<< formatQuery conn template' params
      r <- map fromOnly <$> query conn template' params
      BL.putStrLn $ encode (r :: [Value])
  where
    myAdjustments :: ExpressionAdjustments
    myAdjustments = defaultExpressionAdjustments { eaTableCondition = tc }
      where
        -- only access current user's organization
        tc "organizations" = Just $
          BinOp (Column "organizations" "id") "=" (Literal (BS.pack $ show oid))
        -- this is redundant, but demonstrates recursive validation of
        -- the rules
        tc "users" = Just $ BinOp (Column "users" "organization_id")
          "in"
          (ExpressionSelect $
            Select False [SelectItem (Column "organizations" "id") Nothing]
            (Just "organizations") [] Nothing [] [] Nothing Nothing)
        -- prohibit access to the rest
        tc _ = Just $ Literal "false"
```
