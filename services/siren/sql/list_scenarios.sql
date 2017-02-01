CREATE OR REPLACE FUNCTION list_scenarios(token jsonb)
  RETURNS jsonb AS
$func$
DECLARE
  scenarios jsonb;
BEGIN
  SELECT jsonb_build_object(
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
        )
  INTO scenarios;
  RETURN (
    SELECT jsonb_build_object(
      'result', scenarios,
      'callID', token
    )
  );
END;
$func$ LANGUAGE plpgsql;
