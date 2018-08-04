{- |
Module      :  RQL.Types
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  experimental

RQL AST.
-}

{-# LANGUAGE DeriveGeneric #-}

module RQL.Types where

import qualified Data.ByteString.Char8 as BS
import GHC.Generics

type Identifier = BS.ByteString

data SelectItem = SelectItem { selectItemExpression :: Expression
                             , selectItemAlias :: Maybe Identifier
                             } deriving (Show, Eq, Generic)

data Expression = Function Identifier [Expression]
                -- ^ name, args
                | BinOp Expression Identifier Expression
                -- ^ operand 1, binary operator, operand 2
                | Column Identifier Identifier
                -- ^ relation, column
                | Literal BS.ByteString
                | Row [Expression]
                | ExpressionSelect Select
                deriving (Show, Eq, Generic)

data JoinType = JoinLeft
              | JoinInner
              deriving (Show, Eq, Generic)

data Join = Join { joinType :: JoinType
                 , joinRelation :: Identifier
                 , joinCondition :: Expression
                 } deriving (Show, Eq, Generic)

data SortOrder = ASC | DESC deriving (Show, Eq, Generic)

data Select = Select { selectDistinct :: Bool
                     , selectItems :: [SelectItem]
                     , selectFrom :: Maybe Identifier
                     , selectJoins :: [Join]
                     , selectCondition :: Maybe Expression
                     , selectGrouping :: [Expression]
                     , selectOrdering :: [(Expression, SortOrder)]
                     , selectLimit :: Maybe Int
                     , selectOffset :: Maybe Int
                     } deriving (Show, Eq, Generic)
