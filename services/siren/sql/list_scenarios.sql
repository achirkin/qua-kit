CREATE OR REPLACE FUNCTION list_scenarios()
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
              WHERE scenario.alive
            )
          , '[]'::jsonb
          )
        );
END;
$func$ LANGUAGE plpgsql;
