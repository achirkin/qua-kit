CREATE OR REPLACE FUNCTION delete_scenario(token jsonb, ScID bigint)
  RETURNS jsonb AS
$func$
BEGIN
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to delete a scenario that does not exist!';
  END IF;
  UPDATE scenario
     SET alive = FALSE
   WHERE scenario.id = ScID;
  -- finish!
  RETURN (
      SELECT jsonb_build_object(
        'result', jsonb_build_object
          ( 'ScID'        , ScID
          , 'name'        , (SELECT name FROM scenario WHERE scenario.id = ScID)
          , 'created'     , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          , 'lastmodified', (SELECT round(EXTRACT(epoch FROM MAX(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          ),
        'callID', token
      )
    );
END;
$func$ LANGUAGE plpgsql;
