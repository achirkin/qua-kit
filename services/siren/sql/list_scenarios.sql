CREATE OR REPLACE FUNCTION list_scenarios(userId bigint, authRole text)
  RETURNS jsonb AS
$func$
BEGIN
  RETURN jsonb_build_object(
        'scenarios',
          coalesce
          ( ( SELECT jsonb_agg
              ( jsonb_build_object
                ( 'ScID',         scenario.id
                , 'name',         scenario.name
                , 'created',      (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = scenario.id)
                , 'lastmodified', (SELECT round(EXTRACT(epoch FROM MAX(ts_update))) FROM sc_geometry_history WHERE scenario_id = scenario.id)
                )
              )
              FROM scenario
              WHERE scenario.alive AND
                      ((authRole = 'Student' AND scenario.owner = userId)
                    OR (authRole = 'Local'   AND (scenario.owner IS NULL OR scenario.owner = userId))
                    OR (authRole = 'Admin'   AND TRUE))


            )
          , '[]'::jsonb
          )
        );
END;
$func$ LANGUAGE plpgsql;
