SELECT          anon_1.flavors_created_at       AS anon_1_flavors_created_at,
                anon_1.flavors_updated_at       AS anon_1_flavors_updated_at,
                anon_1.flavors_id               AS anon_1_flavors_id,
                anon_1.flavors_name             AS anon_1_flavors_name,
                anon_1.flavors_memory_mb        AS anon_1_flavors_memory_mb,
                anon_1.flavors_vcpus            AS anon_1_flavors_vcpus,
                anon_1.flavors_root_gb          AS anon_1_flavors_root_gb,
                anon_1. flavors_ephemeral_gb    AS anon_1_flavors_ephemeral_gb,
                anon_1.flavors_flavorid         AS anon_1_flavors_flavorid,
                anon_1.flavors_swap             AS anon_1_flavors_swap,
                anon_1.flavors_rxtx_factor      AS anon_1_flavors_rxtx_factor,
                anon_1.flavors_vcpu_weight      AS anon_1_flavors_vcpu_weight,
                anon_1.flavors_disabled         AS anon_1_flavors_disabled,
                anon_1.flavors_is_public        AS anon_1_flavors_is_public,
                flavor_extra_specs_1.created_at AS flavor_extra_specs_1_created_at,
                flavor_extra_specs_1.updated_at AS flavor_extra_specs_1_updated_at,
                flavor_extra_specs_1.id         AS flavor_extra_specs_1_id,
                flavor_extra_specs_1.`KEY`      AS flavor_extra_specs_1_key,
                flavor_extra_specs_1.value      AS flavor_extra_specs_1_value,
                flavor_extra_specs_1.flavor_id  AS flavor_extra_specs_1_flavor_id
FROM            (
                         SELECT   flavors.created_at   AS flavors_created_at,
                                  flavors.updated_at   AS flavors_updated_at,
                                  flavors.id           AS flavors_id,
                                  flavors.NAME         AS flavors_name,
                                  flavors.memory_mb    AS flavors_memory_mb,
                                  flavors.vcpus        AS flavors_vcpus,
                                  flavors.root_gb      AS flavors_root_gb,
                                  flavors.ephemeral_gb AS flavors_ephemeral_gb,
                                  flavors.flavorid     AS flavors_flavorid,
                                  flavors.swap         AS flavors_swap,
                                  flavors.rxtx_factor  AS flavors_rxtx_factor,
                                  flavors.vcpu_weight  AS flavors_vcpu_weight,
                                  flavors.disabled     AS flavors_disabled,
                                  flavors.is_public    AS flavors_is_public
                         FROM     flavors
                         WHERE    (
                                           flavors.is_public = true
                                  OR       (
                                                    EXISTS
                                                    (
                                                           SELECT 1
                                                           FROM   flavor_projects
                                                           WHERE  flavor_projects.flavor_id = flavors.id
                                                           AND    flavor_projects.project_id = %(project_id_1)s)))
                         AND      flavors.flavorid = %(flavorid_1)s
                         ORDER BY flavors.id ASC limit %(param_1)s) AS anon_1
LEFT OUTER JOIN flavor_extra_specs                                  AS flavor_extra_specs_1
ON              flavor_extra_specs_1.flavor_id = anon_1.flavors_id
ORDER BY        anon_1.flavors_id ASC
