{- |
Module      :  RQL.Validation
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  experimental

Query validation and translation into SQL.
-}

{-# LANGUAGE OverloadedStrings #-}
module RQL.Validation where

import RQL.Types

import           Database.PostgreSQL.Simple.ToField
                 (Action (..))
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Except as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Binary.Builder as BSB
import           Control.Monad
import           Data.Monoid
import           Data.Maybe
import           Data.Functor.Identity


-- * Settings

type Adjusted a = Either BS.ByteString a
-- | A value adjustment, which may fail or alter the value.
type Adjustment a = a -> Adjusted a

-- | Expression modification and validation. This only covers common
-- and basic cases, such as restriction by table, column, function,
-- and operator names.
data ExpressionAdjustments = ExpressionAdjustments
  { eaTable          :: Adjustment Identifier
  -- ^ Table name validation\/restriction\/adjustment.
  , eaTableCondition :: Identifier -> Maybe Expression
  -- ^ Additional condition to add whenever a table is accessed. It is
  -- processed as if it was in the original query.
  , eaFunction       :: Adjustment Identifier
  , eaBinaryOperator :: Adjustment Identifier
  , eaColumn         :: Adjustment (Identifier, Identifier)
  }

-- | Default adjustments: just restrict functions and operators to
-- common and safe ones.
defaultExpressionAdjustments :: ExpressionAdjustments
defaultExpressionAdjustments = ExpressionAdjustments
  Right
  (const Nothing)
  (\i -> if i `elem` [ "not", "coalesce", "count", "sum", "avg", "min", "max"
                     , "every", "array_agg"]
         then Right i
         else Left $ "The function is not allowed: " <> i)
  (\i -> if i `elem` ["and", "or", "=", "like", "in", ">", "<", ">=", "<="]
         then Right i
         else Left $ "The operator is not allowed: " <> i)
  Right


-- * Validation utilities

-- | A validated query part: either a query string or a parameter.
data Validated = VParam Action
               | VQuery BS.ByteString
               deriving (Show)

type Validator = E.ExceptT BS.ByteString (W.Writer [Validated]) ()

runValidator :: Validator -> Either BS.ByteString [Validated]
runValidator v = case runIdentity $ W.runWriterT $ E.runExceptT v of
  (Right (), l) -> Right l
  (Left err, _) -> Left err

-- | Writes a query part.
wQuery :: BS.ByteString -> Validator
wQuery = W.tell . pure . VQuery

-- | Writes a query parameter.
wParam :: Action -> Validator
wParam p = W.tell . pure $ VParam p

-- | Writes an identifier query parameter, such as a table name.
wIdentifier :: BS.ByteString -> Validator
wIdentifier = wParam . EscapeIdentifier

-- | Writes a literal query parameter that should still be escaped,
-- such as a string or a number.
wLiteral :: BS.ByteString -> Validator
wLiteral = wParam . Escape

-- | Writes a plain query parameter, such as a number.
wPlain :: BS.ByteString -> Validator
wPlain = wParam . Plain . BSB.fromByteString

-- | Writes a given 'Validator' in parens.
vParenthesized :: Validator -> Validator
vParenthesized v = wQuery "(" >> v >> wQuery ")"

-- | Intercalates validators with a separator.
vIntercalated :: BS.ByteString -> [Validator] -> Validator
vIntercalated _ [] = pure ()
vIntercalated _ [x] = x
vIntercalated s (x:xs) = x >> wQuery s >> vIntercalated s xs


-- * Validators

-- | Validates an 'Expression'.
vExpression :: ExpressionAdjustments -> Expression -> Validator
vExpression ea (Function fn fa) = do
  wQuery =<< either E.throwError pure (eaFunction ea fn)
  vParenthesized $ vIntercalated ", " $ map (vExpression ea) fa
vExpression ea (BinOp o1 on o2) = vParenthesized $ do
  vExpression ea o1 >> wQuery " "
  wQuery =<< either E.throwError pure (eaBinaryOperator ea on)
  wQuery " " >> vExpression ea o2
vExpression ea (Row es) =
  vParenthesized $ vIntercalated ", " $ map (vExpression ea) es
vExpression ea (ExpressionSelect s) = vParenthesized $ vSelect ea s
vExpression _  (Literal s) = wLiteral s
vExpression ea (Column tn col) = case eaColumn ea (tn, col) of
  Left err -> E.throwError err
  Right (tn', col') -> wIdentifier tn' >> wQuery "." >> wIdentifier col'

-- | Validates a 'SelectItem'.
vSelectItem :: ExpressionAdjustments -> SelectItem -> Validator
vSelectItem ea (SelectItem e a) = do
  vExpression ea e
  case a of
    Nothing -> pure ()
    Just a' -> wQuery " as " >> wIdentifier a'

-- | Validates a 'Join'.
vJoin :: ExpressionAdjustments -> Join -> Validator
vJoin ea (Join jt tn cond) = do
  _ <- wQuery $ case jt of
    JoinLeft -> "left"
    JoinInner -> "inner"
  _ <- wQuery " join "
  _ <- wIdentifier =<< either E.throwError pure (eaTable ea tn)
  _ <- wQuery " on "
  -- The additional conditions also get validated.
  vExpression ea $ maybe cond (BinOp cond "and") (eaTableCondition ea tn)

-- | Validates a 'Select'.
vSelect :: ExpressionAdjustments -> Select -> Validator
vSelect ea (Select distinct items from joins condition grouping ordering limit offset) = do
  wQuery "select "
  when distinct $ wQuery "distinct "
  vIntercalated ", " $ map (vSelectItem ea) items
  maybe' from $ \from' -> do
    wQuery " from "
    wIdentifier =<< either E.throwError pure (eaTable ea from')
  wQuery " "
  vIntercalated " " $ map (vJoin ea) joins
  when (joins /= []) $ wQuery " "
  wQuery "where "
  vExpression ea $ foldr (flip BinOp "and") (Literal "true") $
    catMaybes [condition, eaTableCondition ea =<< from]
  when (grouping /= []) $ do
    wQuery " group by "
    vIntercalated ", " $ map (vExpression ea) grouping
    wQuery " "
  when (ordering /= []) $ do
    wQuery " order by "
    vIntercalated ", " $ map
      (\(e, o) -> vExpression ea e >> wQuery " " >> wQuery (BS.pack $ show o))
      ordering
    wQuery " "
  maybe' limit $ \l -> wQuery " limit " >> wPlain (BS.pack $ show l)
  maybe' offset $ \o -> wQuery " offset " >> wPlain (BS.pack $ show o)
  where
    maybe' = flip (maybe (pure ()))
