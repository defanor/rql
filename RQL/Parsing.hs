{- |
Module      :  RQL.Parsing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  experimental

Query parsing.

Apart from being restricted (simplified), the grammar differs from
that of SQL most notably in binary operator applications always being
parenthesized, and column names always being accompanied by table
names. Very briefly and informally, it can be described as:

> select
>   [distinct]
>   [(<expression> [as <identifier>]) [, ...]]
>   [from <identifier>]
>   [(inner|left) join <identifier> on <expression>]
>   [where <expression>]
>   [group by <expression> [, ...]]
>   [order by <expression> [asc|desc] [, ...]]
>   [limit <number>]
>   [offset <number>]

-}

{-# LANGUAGE OverloadedStrings #-}
module RQL.Parsing where

import RQL.Types

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Monoid

-- * Utility parsers

-- | Reads one or more 'space' characters.
spacing :: Parser String
spacing = many1 space <?> "spacing"

-- | Reads comma-separated values.
commaSep :: Parser a -> Parser [a]
commaSep p  = p `sepBy` ("," *> many space) <?> "comma-separated"

-- | Reads comma-separated values, at least one.
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` ("," *> many space) <?> "comma-separated"

-- | Applies a given parser between parens.
parenthesized :: Parser p -> Parser p
parenthesized p = "(" *> many space *> p <* many space <* ")" <?> "parenthesized"

-- | @identifier = (inClass "a-z_") ( inClass "a-z0-9_" )*@
identifier :: Parser Identifier
identifier = BS.pack
  <$> ((:) <$> satisfy (inClass "a-z_") <*> many (satisfy (inClass "a-z0-9_")))
  <?> "identifier"

-- | @quoted-string = "'" ( any-character - "'" | "''" )* "'"@
quotedString :: Parser BS.ByteString
quotedString = "'" 
  *> (BS.pack <$> manyTill (("''" *> pure '\'') <|> notChar '\'') "'")
  <?> "quoted string"

-- | @numeric = [ "-" ] digit* [ "." digit* ]@
numeric :: Parser BS.ByteString
numeric = do
  sign <- "-" <|> pure "" <?> "sign"
  digits <- BS.pack <$> many1 (satisfy (inClass "0-9")) <?> "digits"
  fractional <- (BS.append <$> string "."
                 <*> (BS.pack <$> many1 (satisfy (inClass "0-9"))))
    <|> pure ""
    <?> "fractional part"
  pure $ sign <> digits <> fractional

-- * Expression parsers

-- | @literal = "null" | "true" | "false" | numeric | quoted-string@
literal :: Parser Expression
literal = Literal
  <$> ("null" <|> "true" <|> "false" <|> numeric <|> quotedString)
  <?> "literal"

-- | @column = identifier "." identifier@
column :: Parser Expression
column = Column <$> (identifier <* ".") <*> identifier <?> "column"

-- | @select-item = expression [ " as " identifier ]@
selectItem :: Parser SelectItem
selectItem = SelectItem
  <$> expression
  <*> (optional (spacing *> "as" *> spacing *> identifier) <?> "item alias")
  <?> "select item"

-- | @function = identifier "(" [ expression ( ", " expression )* ] ")"@
function :: Parser Expression
function = Function
  <$> identifier <*> parenthesized (commaSep expression) <?> "function"

-- | > binary-operator = expression " " operator-name " " expression
--
-- > operator-name = ( inClass "a-z=~<>+-@^%&*/!|#" )*
binOp :: Parser Expression
binOp = parenthesized
  (BinOp
    <$> ((expression <?> "first operand") <* spacing)
    <*> (BS.pack <$> manyTill (satisfy (inClass "a-z=~<>+-@^%&*/!|#`")) spacing
          <?> "binary operator")
    <*> (expression <?> "second operand"))
  <?> "binary operation"

-- | @row = "(" expression ( ", " expression )* ")"@
row :: Parser Expression
row = Row <$> parenthesized (commaSep1 expression) <?> "row"

-- | @expression = function
-- | row
-- | binary-operator
-- | literal
-- | column
-- | "(" select ")"@
expression :: Parser Expression
expression = choice
  [ function, row, binOp, literal, column
  , parenthesized (ExpressionSelect <$> select <?> "nested select")]
  <?> "expression"

-- | @join = ("inner" | "left") " join " identifier " on " expression@
join :: Parser Join
join = Join
       <$> ((("inner" *> pure JoinInner) <|> ("left" *> pure JoinLeft)
              <?> "join type")
             <* spacing <* "join" <* spacing)
       <*> (identifier <* spacing <* "on" <* spacing)
       <*> (expression <?> "condition")
       <?> "join"

-- | @sort-order = "asc" | "desc"@
sortOrder :: Parser SortOrder
sortOrder = (("asc" *> pure ASC) <|> ("desc" *> pure DESC))
            <?> "sort order"

-- | @select = "select "
--     ["distinct "]
--     select-item ( ", " select-item )*
--     [ " from " identifier ]
--     join*
--     [ " where " expression ]
--     [ " group by " expression ( ", " expression )* ]
--     [ " order by " expression [" " sort-order] ( ", " expression [" " sort-order] )* ]
--     [ " limit " decimal ]
--     [ " offset " decimal ]@
select :: Parser Select
select = do
  _ <- "select"
  _ <- spacing
  distinct <- ("distinct" *> spacing *> pure True) <|> pure False
  items <- commaSep1 selectItem <?> "select items"
  from <- optional (spacing *> "from" *> spacing *> identifier) <?> "from"
  joins <- (spacing *> sepBy1 join spacing) <|> pure [] <?> "joins"
  condition <- optional (spacing *> "where" *> (spacing *> expression))
               <?> "condition"
  grouping <- (spacing *> "group" *> spacing *> "by" *> spacing
                *> commaSep1 expression)
              <|> pure [] <?> "grouping"
  ordering <- (spacing *> "order" *> spacing *> "by" *> spacing
               *> commaSep1 ((,) <$> expression <*> ((spacing *> sortOrder)
                                                     <|> pure ASC)))
              <|> pure [] <?> "ordering"
  limit <- optional (spacing *> "limit" *> spacing *> decimal) <?> "limit"
  offset <- optional (spacing *> "offset" *> spacing *> decimal)  <?> "offset"
  pure $
    Select distinct items from joins condition grouping ordering limit offset
