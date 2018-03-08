select anon_1.flavors_created_at as anon_1_flavors_created_at,
       anon_1.flavors_updated_at as anon_1_flavors_updated_at,
       anon_1.flavors_id as anon_1_flavors_id,
       anon_1.flavors_name as anon_1_flavors_name,
       anon_1.flavors_memory_mb as anon_1_flavors_memory_mb,
       anon_1.flavors_vcpus as anon_1_flavors_vcpus,
       anon_1.flavors_root_gb as anon_1_flavors_root_gb,
       anon_1.flavors_ephemeral_gb as anon_1_flavors_ephemeral_gb,
       anon_1.flavors_flavorid as anon_1_flavors_flavorid,
       anon_1.flavors_swap as anon_1_flavors_swap,
       anon_1.flavors_rxtx_factor as anon_1_flavors_rxtx_factor,
       anon_1.flavors_vcpu_weight as anon_1_flavors_vcpu_weight,
       anon_1.flavors_disabled as anon_1_flavors_disabled,
       anon_1.flavors_is_public as anon_1_flavors_is_public,
       flavor_extra_specs_1.created_at as flavor_extra_specs_1_created_at,
       flavor_extra_specs_1.updated_at as flavor_extra_specs_1_updated_at,
       flavor_extra_specs_1.id as flavor_extra_specs_1_id,
       flavor_extra_specs_1."key" as flavor_extra_specs_1_key,
       flavor_extra_specs_1.value as flavor_extra_specs_1_value,
       flavor_extra_specs_1.flavor_id as flavor_extra_specs_1_flavor_id
from (select flavors.created_at as flavors_created_at,
             flavors.updated_at as flavors_updated_at,
             flavors.id as flavors_id,
             flavors.name as flavors_name,
             flavors.memory_mb as flavors_memory_mb,
             flavors.vcpus as flavors_vcpus,
             flavors.root_gb as flavors_root_gb,
             flavors.ephemeral_gb as flavors_ephemeral_gb,
             flavors.flavorid as flavors_flavorid,
             flavors.swap as flavors_swap,
             flavors.rxtx_factor as flavors_rxtx_factor,
             flavors.vcpu_weight as flavors_vcpu_weight,
             flavors.disabled as flavors_disabled,
             flavors.is_public as flavors_is_public
      from flavors
      where (flavors.is_public = 1
             or (exists (select 1
                         from flavor_projects
                         where flavor_projects.flavor_id = flavors.id
                               and flavor_projects.project_id = ?)))
            and (flavors.flavorid > ?
                 or flavors.flavorid = ?
                    and flavors.id > ?)
      order by flavors.flavorid asc, flavors.id asc
      offset ? rows
      limit ?)
     as anon_1
     left join flavor_extra_specs as flavor_extra_specs_1
     on flavor_extra_specs_1.flavor_id = anon_1.flavors_id
order by anon_1.flavors_flavorid asc, anon_1.flavors_id asc
