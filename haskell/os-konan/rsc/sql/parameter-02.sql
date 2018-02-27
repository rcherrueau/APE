INSERT INTO request_specs
            (
                        created_at,
                        updated_at,
                        instance_uuid,
                        spec
            )
            VALUES
            (
                        %(created_at)s,
                        %(updated_at)s,
                        %(instance_uuid)s,
                        %(spec)s
            )
