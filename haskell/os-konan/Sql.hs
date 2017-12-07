module Sql where

import Language.SQL.SimpleSQL.Parser
import Language.SQL.SimpleSQL.Syntax

import Data.Either (isRight)
import Data.Maybe (fromMaybe)

-- | Utils functions for SQL query.
class UtilsQuery q where
  -- | Lists all references to tables that appear in a `q`.
  tableRefs :: q -> [TableRef]

  -- | Lists all sub-queries that appear in a `q`.
  listSubqueries :: q -> [QueryExpr]

  -- | Determines whether a `q` has, at least, one sub-query.
  hasSubquery :: q -> Bool
  hasSubquery = not . null . listSubqueries

instance (UtilsQuery q) => UtilsQuery [q] where
  tableRefs = concatMap tableRefs
  listSubqueries = concatMap listSubqueries

instance UtilsQuery SortSpec where
  tableRefs (SortSpec vExpr _ _) = tableRefs vExpr
  listSubqueries (SortSpec vExpr _ _) = listSubqueries vExpr

instance UtilsQuery InPredValue where
  tableRefs (InList q) = tableRefs q
  tableRefs (InQueryExpr q) = tableRefs q

  listSubqueries (InList vExprs) = listSubqueries vExprs
  listSubqueries (InQueryExpr qExpr) = listSubqueries qExpr

instance UtilsQuery JoinCondition where
  tableRefs (JoinOn    v) = tableRefs v
  tableRefs (JoinUsing names) = map (TRSimple . (: [])) names

  listSubqueries (JoinOn vExpr) = listSubqueries vExpr
  listSubqueries _ = []

instance UtilsQuery TableRef where
  tableRefs tr = [tr]

  listSubqueries (TRSimple _) = []
  listSubqueries (TRJoin tRef _ _ tRef' mayJCond) =
    listSubqueries tRef ++ listSubqueries tRef' ++ maybe [] listSubqueries mayJCond
  listSubqueries (TRParens tRef) = listSubqueries tRef
  listSubqueries (TRAlias tRef _) = listSubqueries tRef
  listSubqueries (TRQueryExpr qExpr) = listSubqueries qExpr
  listSubqueries (TRFunction _ vExprs) = listSubqueries vExprs
  listSubqueries (TRLateral tRef) = listSubqueries tRef

instance UtilsQuery GroupingExpr where
  tableRefs (GroupingParens gs) = concatMap tableRefs gs
  tableRefs (Cube gs) = concatMap tableRefs gs
  tableRefs (Rollup gs) = concatMap tableRefs gs
  tableRefs (GroupingSets gs) = concatMap tableRefs gs
  tableRefs (SimpleGroup g) = tableRefs g

  listSubqueries (SimpleGroup vExpr) = listSubqueries vExpr
  listSubqueries (GroupingParens gExprs) = listSubqueries gExprs
  listSubqueries (Cube gExprs) = listSubqueries gExprs
  listSubqueries (Rollup gExprs) = listSubqueries gExprs
  listSubqueries (GroupingSets gExprs) = listSubqueries gExprs

instance UtilsQuery ValueExpr where
  tableRefs (Iden col) =
    -- identifier with parts separated by dots, e.g., schema.table.col, table.col, col
    [TRSimple (init col)]
  tableRefs (App _ args) =
    -- Function application
    concatMap tableRefs args
  tableRefs (AggregateApp _ _ args orderby filter_) =
    -- Aggregate function application (add orderby, filter to regular function)
    concatMap tableRefs args
    ++ concatMap tableRefs orderby
    ++ maybe [] tableRefs filter_
  tableRefs (AggregateAppGroup _ args group) =
    -- Aggregate with within group
    concatMap tableRefs args ++ concatMap tableRefs group
  tableRefs (WindowApp _ args partitions orderby _) =
    concatMap tableRefs args
    ++ concatMap tableRefs partitions
    ++ concatMap tableRefs orderby
  tableRefs (BinOp v _ v') = tableRefs v ++ tableRefs v'
  tableRefs (PrefixOp _ v) = tableRefs v
  tableRefs (PostfixOp _ v) = tableRefs v
  tableRefs (SpecialOp _ v) = concatMap tableRefs v
  tableRefs (SpecialOpK _ arg1 args) =
    maybe [] tableRefs arg1 ++ concatMap (tableRefs . snd) args
  tableRefs (Case if_ then_ else_) =
    maybe [] tableRefs if_
    ++ maybe [] tableRefs else_
    ++ concatMap tableRefs' then_
    where
      tableRefs' :: ([ValueExpr], ValueExpr) -> [TableRef]
      tableRefs' (vs, v) = tableRefs vs ++ tableRefs v
  tableRefs (Parens v) = tableRefs v
  tableRefs (Cast v _) = tableRefs v
  tableRefs (SubQueryExpr _ q) = tableRefs q
  tableRefs (In _ v predicat) = tableRefs v ++ tableRefs predicat
  tableRefs (QuantifiedComparison v _ _ q) = tableRefs v ++ tableRefs q
  tableRefs (Match v _ q) = tableRefs v ++ tableRefs q
  tableRefs (Array v vs) = tableRefs v ++ concatMap tableRefs vs
  tableRefs (ArrayCtor q) = tableRefs q
  tableRefs (Escape v _) = tableRefs v
  tableRefs (UEscape v _) = tableRefs v
  tableRefs (Collate v _) = tableRefs v
  tableRefs (MultisetBinOp v _ _ v') = tableRefs v ++ tableRefs v'
  tableRefs (MultisetCtor vs) = concatMap tableRefs vs
  tableRefs (MultisetQueryCtor q) = tableRefs q
  tableRefs (VEComment _ v) = tableRefs v
  tableRefs _ = []

  listSubqueries (App _ vExprs) =
    listSubqueries vExprs
  listSubqueries (AggregateApp _ _ vExprs specs mayVExpr) =
    listSubqueries vExprs
    ++ listSubqueries specs
    ++ maybe [] listSubqueries mayVExpr
  listSubqueries (AggregateAppGroup _ vExprs specs) =
    listSubqueries vExprs
    ++ listSubqueries specs
  listSubqueries (WindowApp _ vExprs vExprs' specs _) =
    listSubqueries vExprs
    ++ listSubqueries vExprs'
    ++ listSubqueries specs
  listSubqueries (BinOp vExpr _ vExpr') =
    listSubqueries vExpr ++ listSubqueries vExpr'
  listSubqueries (PrefixOp _ vExpr) = listSubqueries vExpr
  listSubqueries (PostfixOp _ vExpr) = listSubqueries vExpr
  listSubqueries (SpecialOp _ vExprs) = listSubqueries vExprs
  listSubqueries (SpecialOpK _ mayVExpr tupleVExprs) =
    maybe [] listSubqueries mayVExpr
    ++ concatMap (listSubqueries . snd) tupleVExprs
  listSubqueries (Case mayVExpr when mayVExpr') =
    maybe [] listSubqueries mayVExpr
    ++ maybe [] listSubqueries mayVExpr'
    ++ concatMap listSubquerys' when
    where
      listSubquerys' :: ([ValueExpr], ValueExpr) -> [QueryExpr]
      listSubquerys' (vs, v) = listSubqueries vs ++ listSubqueries v
  listSubqueries (Parens vExpr) = listSubqueries vExpr
  listSubqueries (Cast vExpr _) = listSubqueries vExpr
  listSubqueries (In _ vExpr predVal) = listSubqueries vExpr ++ listSubqueries predVal
  listSubqueries (QuantifiedComparison vExpr _ _ qExpr) = listSubqueries vExpr ++ listSubqueries qExpr
  listSubqueries (Match vExpr _ qExpr) = listSubqueries vExpr ++ listSubqueries qExpr
  listSubqueries (Array vExpr vExprs) = listSubqueries vExpr ++ listSubqueries vExprs
  listSubqueries (ArrayCtor qExpr) = listSubqueries qExpr
  listSubqueries (Escape vExpr _) = listSubqueries vExpr
  listSubqueries (UEscape vExpr _) = listSubqueries vExpr
  listSubqueries (Collate vExpr _) = listSubqueries vExpr
  listSubqueries (MultisetBinOp vExpr _ _ vExpr') = listSubqueries vExpr ++ listSubqueries vExpr'
  listSubqueries (MultisetCtor vExprs) = listSubqueries vExprs
  listSubqueries (MultisetQueryCtor qExpr) =  listSubqueries qExpr
  listSubqueries (VEComment _ vExpr) = listSubqueries vExpr
  listSubqueries (SubQueryExpr _ qExpr) = qExpr : listSubqueries qExpr
  listSubqueries _ = []

instance UtilsQuery QueryExpr where
  tableRefs (Select _ selects from where_ groups having orderby offset limit) =
    -- Regular SELECT:
    -- SELECT `selects` FROM `from` WHERE `where`
    -- GROUP BY `group` HAVING `having` ORDER BY `orderby`
    -- OFFSET `offset` LIMIT `limit`
    concatMap selectsRefs selects ++ from
    ++ maybe [] tableRefs where_
    ++ concatMap tableRefs groups
    ++ maybe [] tableRefs having
    ++ concatMap tableRefs orderby
    ++ maybe [] tableRefs offset
    ++ maybe [] tableRefs limit
    where
      -- Do not take care of alias (`Maybe Name`) because they not
      -- represent a `TableRef` but a Column reference
      selectsRefs :: (ValueExpr, Maybe Name) -> [TableRef]
      selectsRefs = tableRefs . fst
  tableRefs (CombineQueryExpr qExpr _ _ _ qExpr') =
    -- qExpr UNION qExpr'
    tableRefs qExpr ++ tableRefs qExpr'
  tableRefs (With _ views qExpr) =
    -- WITH <alias1> AS (sql_subquery1)  - view
    -- WITH <alias2> AS (sql_subquery2)  - view
    -- SELECT column_list FROM alias1, alias2[, tables]
    map (\(alias, qExpr') -> (TRAlias (TRQueryExpr qExpr') alias)) views
    ++ tableRefs qExpr
  tableRefs (Values _) = []
  tableRefs (Table name) =
    -- `TABLE name` is completely equivalent to `SELECT * FROM name`
    [TRSimple name]
  tableRefs (QEComment _ qExpr) = tableRefs qExpr

  listSubqueries (Select _ tupleVExprs tRefs mayVExpr1 gExprs mayVExpr2 specs mayVExpr3 mayVExpr4) =
    concatMap (listSubqueries . fst) tupleVExprs
    ++ listSubqueries tRefs
    ++ maybe [] listSubqueries mayVExpr1
    ++ listSubqueries gExprs
    ++ maybe [] listSubqueries mayVExpr2
    ++ maybe [] listSubqueries mayVExpr3
    ++ listSubqueries specs
    ++ maybe [] listSubqueries mayVExpr4
  listSubqueries (CombineQueryExpr qExpr _ _ _ qExpr') = [qExpr, qExpr']
  listSubqueries (With _ tupleQExprs qExpr) =
    map snd tupleQExprs ++ listSubqueries qExpr
  listSubqueries (Values vExprss) = listSubqueries vExprss
  listSubqueries (QEComment _ qExpr) = listSubqueries qExpr
  listSubqueries _ = []

-- | Determines whether a `QueryExpr` gets, at least, one correlated
-- sub-query. A correlated sub-query is a sub-query that runs against
-- each row returned by the main query.
isCorrelated :: QueryExpr -> Bool
isCorrelated = any isCorrelated' . listSubqueries
  -- To determine if `QueryExpr` contains correlated subqueries, the
  -- following test first find all sub-queries. Then, determines if
  -- the sub-query is a correlated one. To do so, it compares tables
  -- referenced in the query (`wholeCtx`) with tables referenced in
  -- the from clause (`fromCtx`). If the query contains reference to
  -- tables that do not appear in the from clause, then the query use
  -- information from the main query, and hence it's a correlated one.
  where
    isCorrelated' :: QueryExpr -> Bool
    isCorrelated' q =
      let fromCtx      = concatMap tableNames (fromClauseRef q)
          wholeCtx     = concatMap tableNames (tableRefs q)
          notInFromCtx = filter (`notElem` fromCtx) wholeCtx
      in not (null notInFromCtx)

    -- | List all tables reference in the from clause of `QueryExpr`.
    fromClauseRef :: QueryExpr -> [TableRef]
    fromClauseRef (Select _ _ from _ _ _ _ _ _) = from
    fromClauseRef (CombineQueryExpr qExpr _ _ _ qExpr') = fromClauseRef qExpr ++ fromClauseRef qExpr'
    fromClauseRef (With _ views qExpr) =
      map (\(alias, qExpr') -> (TRAlias (TRQueryExpr qExpr') alias)) views
      ++ fromClauseRef qExpr
    fromClauseRef (Values _) = []
    fromClauseRef (Table names) = [TRSimple names]
    fromClauseRef (QEComment _ qExpr) = fromClauseRef qExpr

-- | Determines whether a String is a parsable query expression.
canParse :: String -> Bool
canParse = isRight . parseQueryExpr MySQL "" Nothing

-- readSQLs :: FilePath -> IO [String]
-- readSQLs filePath = lines <$> readFile filePath

-- | Returns the identifier name of a `TableRef`.
tableNames :: TableRef -> [Name]
tableNames (TRSimple []   ) = []
tableNames (TRSimple names) = [last names]  -- from name, from schema.name
tableNames (TRJoin tref _ _ tref' _) = tableNames tref ++ tableNames tref'
tableNames (TRParens tref) = tableNames tref
tableNames (TRAlias _ (Alias alias mayAliases)) = alias : fromMaybe [] mayAliases
-- tableNames (TRQueryExpr qExpr) = concatMap tableNames (fromClauseRef qExpr)
tableNames (TRQueryExpr qExpr) = concatMap tableNames (tableRefs qExpr)
tableNames (TRFunction names _) = [ head names ]
tableNames (TRLateral tref) = tableNames tref
