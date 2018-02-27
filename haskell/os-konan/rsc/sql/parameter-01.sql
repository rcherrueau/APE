SELECT USER.enabled            AS user_enabled,
       USER.id                 AS user_id,
       USER.domain_id          AS user_domain_id,
       USER.extra              AS user_extra,
       USER.default_project_id AS user_default_project_id,
       USER.created_at         AS user_created_at,
       USER.last_active_at     AS user_last_active_at
FROM   USER
WHERE  USER.id = %(param_1)s
