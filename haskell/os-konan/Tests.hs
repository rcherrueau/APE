module Tests where

import Sql
import Language.SQL.SimpleSQL.Syntax


-- SQL Test

-- | Example of a correlated subquery
--
--  Original query is:
--  SELECT col1 FROM tab1
--  WHERE EXISTS (SELECT 1 FROM tab2 WHERE col2 = tab1.col2)
correlatedQ :: QueryExpr
correlatedQ = Select {
  qeSetQuantifier = SQDefault,
  qeSelectList = [(Iden [Name "col1"],Nothing)],
  qeFrom = [TRSimple [Name "tab1"]],
  qeWhere = Just
    (SubQueryExpr SqExists Select {
        qeSetQuantifier = SQDefault,
        qeSelectList = [(NumLit "1",Nothing)],
        qeFrom = [TRSimple [Name "tab2"]],
        qeWhere = Just (BinOp (Iden [Name "col2"]) [Name "="] (Iden [Name "tab1",Name "col2"])),
        qeGroupBy = [],
        qeHaving = Nothing,
        qeOrderBy = [],
        qeOffset = Nothing,
        qeFetchFirst = Nothing }),
    qeGroupBy = [],
    qeHaving = Nothing,
    qeOrderBy = [],
    qeOffset = Nothing,
    qeFetchFirst = Nothing }

-- | Example of a correlated subquery (from openstack flavor list)
--
-- Original query is:
-- SELECT anon_1.flavors_created_at AS anon_1_flavors_created_at,
-- anon_1.flavors_updated_at AS anon_1_flavors_updated_at,
-- anon_1.flavors_id AS anon_1_flavors_id, anon_1.flavors_name AS
-- anon_1_flavors_name, anon_1.flavors_memory_mb AS
-- anon_1_flavors_memory_mb, anon_1.flavors_vcpus AS
-- anon_1_flavors_vcpus, anon_1.flavors_root_gb AS
-- anon_1_flavors_root_gb, anon_1.flavors_ephemeral_gb AS
-- anon_1_flavors_ephemeral_gb, anon_1.flavors_flavorid AS
-- anon_1_flavors_flavorid, anon_1.flavors_swap AS anon_1_flavors_swap,
-- anon_1.flavors_rxtx_factor AS anon_1_flavors_rxtx_factor,
-- anon_1.flavors_vcpu_weight AS anon_1_flavors_vcpu_weight,
-- anon_1.flavors_disabled AS anon_1_flavors_disabled,
-- anon_1.flavors_is_public AS anon_1_flavors_is_public,
-- flavor_extra_specs_1.created_at AS flavor_extra_specs_1_created_at,
-- flavor_extra_specs_1.updated_at AS flavor_extra_specs_1_updated_at,
-- flavor_extra_specs_1.id AS flavor_extra_specs_1_id,
-- flavor_extra_specs_1.key AS flavor_extra_specs_1_key,
-- flavor_extra_specs_1.value AS flavor_extra_specs_1_value,
-- flavor_extra_specs_1.flavor_id AS flavor_extra_specs_1_flavor_id
-- FROM (SELECT flavors.created_at AS flavors_created_at,
--   flavors.updated_at AS flavors_updated_at, flavors.id AS flavors_id,
--   flavors.name AS flavors_name, flavors.memory_mb AS flavors_memory_mb,
--   flavors.vcpus AS flavors_vcpus, flavors.root_gb AS flavors_root_gb,
--   flavors.ephemeral_gb AS flavors_ephemeral_gb, flavors.flavorid AS
--   flavors_flavorid, flavors.swap AS flavors_swap, flavors.rxtx_factor AS
--   flavors_rxtx_factor, flavors.vcpu_weight AS flavors_vcpu_weight,
--   flavors.disabled AS flavors_disabled, flavors.is_public AS
--   flavors_is_public
--   FROM flavors
--   WHERE flavors.is_public = true OR
--   (EXISTS (SELECT 1
--     FROM flavor_projects
--     WHERE
--     flavor_projects.flavor_id = flavors.id AND flavor_projects.project_id
--     = ?)) ORDER BY flavors.flavorid ASC, flavors.id ASC
--   LIMIT ?) AS anon_1 LEFT OUTER JOIN flavor_extra_specs AS
-- flavor_extra_specs_1 ON flavor_extra_specs_1.flavor_id =
-- anon_1.flavors_id ORDER BY anon_1.flavors_flavorid ASC,
-- anon_1.flavors_id ASC
correlatedQ' :: QueryExpr
correlatedQ' = Select {qeSetQuantifier = SQDefault, qeSelectList = [(Iden [Name "anon_1",Name "flavors_created_at"],Just (Name "anon_1_flavors_created_at")),(Iden [Name "anon_1",Name "flavors_updated_at"],Just (Name "anon_1_flavors_updated_at")),(Iden [Name "anon_1",Name "flavors_id"],Just (Name "anon_1_flavors_id")),(Iden [Name "anon_1",Name "flavors_name"],Just (Name "anon_1_flavors_name")),(Iden [Name "anon_1",Name "flavors_memory_mb"],Just (Name "anon_1_flavors_memory_mb")),(Iden [Name "anon_1",Name "flavors_vcpus"],Just (Name "anon_1_flavors_vcpus")),(Iden [Name "anon_1",Name "flavors_root_gb"],Just (Name "anon_1_flavors_root_gb")),(Iden [Name "anon_1",Name "flavors_ephemeral_gb"],Just (Name "anon_1_flavors_ephemeral_gb")),(Iden [Name "anon_1",Name "flavors_flavorid"],Just (Name "anon_1_flavors_flavorid")),(Iden [Name "anon_1",Name "flavors_swap"],Just (Name "anon_1_flavors_swap")),(Iden [Name "anon_1",Name "flavors_rxtx_factor"],Just (Name "anon_1_flavors_rxtx_factor")),(Iden [Name "anon_1",Name "flavors_vcpu_weight"],Just (Name "anon_1_flavors_vcpu_weight")),(Iden [Name "anon_1",Name "flavors_disabled"],Just (Name "anon_1_flavors_disabled")),(Iden [Name "anon_1",Name "flavors_is_public"],Just (Name "anon_1_flavors_is_public")),(Iden [Name "flavor_extra_specs_1",Name "created_at"],Just (Name "flavor_extra_specs_1_created_at")),(Iden [Name "flavor_extra_specs_1",Name "updated_at"],Just (Name "flavor_extra_specs_1_updated_at")),(Iden [Name "flavor_extra_specs_1",Name "id"],Just (Name "flavor_extra_specs_1_id")),(Iden [Name "flavor_extra_specs_1",Name "key"],Just (Name "flavor_extra_specs_1_key")),(Iden [Name "flavor_extra_specs_1",Name "value"],Just (Name "flavor_extra_specs_1_value")),(Iden [Name "flavor_extra_specs_1",Name "flavor_id"],Just (Name "flavor_extra_specs_1_flavor_id"))], qeFrom = [TRJoin (TRAlias (TRQueryExpr (Select {qeSetQuantifier = SQDefault, qeSelectList = [(Iden [Name "flavors",Name "created_at"],Just (Name "flavors_created_at")),(Iden [Name "flavors",Name "updated_at"],Just (Name "flavors_updated_at")),(Iden [Name "flavors",Name "id"],Just (Name "flavors_id")),(Iden [Name "flavors",Name "name"],Just (Name "flavors_name")),(Iden [Name "flavors",Name "memory_mb"],Just (Name "flavors_memory_mb")),(Iden [Name "flavors",Name "vcpus"],Just (Name "flavors_vcpus")),(Iden [Name "flavors",Name "root_gb"],Just (Name "flavors_root_gb")),(Iden [Name "flavors",Name "ephemeral_gb"],Just (Name "flavors_ephemeral_gb")),(Iden [Name "flavors",Name "flavorid"],Just (Name "flavors_flavorid")),(Iden [Name "flavors",Name "swap"],Just (Name "flavors_swap")),(Iden [Name "flavors",Name "rxtx_factor"],Just (Name "flavors_rxtx_factor")),(Iden [Name "flavors",Name "vcpu_weight"],Just (Name "flavors_vcpu_weight")),(Iden [Name "flavors",Name "disabled"],Just (Name "flavors_disabled")),(Iden [Name "flavors",Name "is_public"],Just (Name "flavors_is_public"))], qeFrom = [TRSimple [Name "flavors"]], qeWhere = Just (BinOp (BinOp (Iden [Name "flavors",Name "is_public"]) [Name "="] (Iden [Name "true"])) [Name "or"] (Parens (SubQueryExpr SqExists (Select {qeSetQuantifier = SQDefault, qeSelectList = [(NumLit "1",Nothing)], qeFrom = [TRSimple [Name "flavor_projects"]], qeWhere = Just (BinOp (BinOp (Iden [Name "flavor_projects",Name "flavor_id"]) [Name "="] (Iden [Name "flavors",Name "id"])) [Name "and"] (BinOp (Iden [Name "flavor_projects",Name "project_id"]) [Name "="] Parameter)), qeGroupBy = [], qeHaving = Nothing, qeOrderBy = [], qeOffset = Nothing, qeFetchFirst = Nothing})))), qeGroupBy = [], qeHaving = Nothing, qeOrderBy = [SortSpec (Iden [Name "flavors",Name "flavorid"]) Asc NullsOrderDefault,SortSpec (Iden [Name "flavors",Name "id"]) Asc NullsOrderDefault], qeOffset = Nothing, qeFetchFirst = Just Parameter})) (Alias (Name "anon_1") Nothing)) False JLeft (TRAlias (TRSimple [Name "flavor_extra_specs"]) (Alias (Name "flavor_extra_specs_1") Nothing)) (Just (JoinOn (BinOp (Iden [Name "flavor_extra_specs_1",Name "flavor_id"]) [Name "="] (Iden [Name "anon_1",Name "flavors_id"]))))], qeWhere = Nothing, qeGroupBy = [], qeHaving = Nothing, qeOrderBy = [SortSpec (Iden [Name "anon_1",Name "flavors_flavorid"]) Asc NullsOrderDefault,SortSpec (Iden [Name "anon_1",Name "flavors_id"]) Asc NullsOrderDefault], qeOffset = Nothing, qeFetchFirst = Nothing}

-- Following tests should return true

testHasSubqueries :: Bool
testHasSubqueries = all hasSubquery [correlatedQ, correlatedQ']

testIsCorrelated :: Bool
testIsCorrelated = all isCorrelated [correlatedQ, correlatedQ']
