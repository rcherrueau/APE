select instance_types.created_at as instance_types_created_at,
       instance_types.updated_at as instance_types_updated_at,
       instance_types.deleted_at as instance_types_deleted_at,
       instance_types.deleted as instance_types_deleted,
       instance_types.id as instance_types_id,
       instance_types.name as instance_types_name,
       instance_types.memory_mb as instance_types_memory_mb,
       instance_types.vcpus as instance_types_vcpus,
       instance_types.root_gb as instance_types_root_gb,
       instance_types.ephemeral_gb as instance_types_ephemeral_gb,
       instance_types.flavorid as instance_types_flavorid,
       instance_types.swap as instance_types_swap,
       instance_types.rxtx_factor as instance_types_rxtx_factor,
       instance_types.vcpu_weight as instance_types_vcpu_weight,
       instance_types.disabled as instance_types_disabled,
       instance_types.is_public as instance_types_is_public,
       instance_type_extra_specs_1.created_at as instance_type_extra_specs_1_created_at,
       instance_type_extra_specs_1.updated_at as instance_type_extra_specs_1_updated_at,
       instance_type_extra_specs_1.deleted_at as instance_type_extra_specs_1_deleted_at,
       instance_type_extra_specs_1.deleted as instance_type_extra_specs_1_deleted,
       instance_type_extra_specs_1.id as instance_type_extra_specs_1_id,
       instance_type_extra_specs_1."key" as instance_type_extra_specs_1_key,
       instance_type_extra_specs_1.value as instance_type_extra_specs_1_value,
       instance_type_extra_specs_1.instance_type_id as instance_type_extra_specs_1_instance_type_id
from instance_types
     left join instance_type_extra_specs as instance_type_extra_specs_1
     on instance_type_extra_specs_1.instance_type_id = instance_types.id
        and instance_type_extra_specs_1.deleted = ?
where instance_types.deleted = ?
      and (instance_types.is_public = 1
           or (exists (select 1
                       from instance_type_projects
                       where instance_type_projects.instance_type_id = instance_types.id
                             and instance_type_projects.deleted = ?
                             and instance_type_projects.project_id = ?)))
order by instance_types.flavorid asc, instance_types.id asc
