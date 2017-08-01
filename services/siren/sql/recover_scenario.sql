CREATE OR REPLACE FUNCTION recover_scenario(ScID bigint, userId bigint, authRole text)
  RETURNS jsonb AS
$func$
BEGIN

  IF (CASE authRole
          WHEN 'Student' THEN (SELECT scenario.owner != userId FROM scenario WHERE scenario.id = ScID)
          WHEN 'Local'   THEN (SELECT CASE WHEN scenario.owner IS NULL THEN FALSE
                                           ELSE scenario.owner != userId
                                      END
                                 FROM scenario
                                WHERE scenario.id = ScID)
          WHEN 'Admin'   THEN FALSE
          ELSE                TRUE
      END)
  THEN
    RAISE EXCEPTION 'You do not have rights to create new scenarios.'
          USING HINT = 'You must have authRole = "Admin" or "Local"';
  END IF;


  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to recover a scenario that has not ever existed!';
  END IF;
  UPDATE scenario
     SET alive = TRUE
   WHERE scenario.id = ScID;
  -- finish!
  RETURN jsonb_build_object
          ( 'ScID'        , ScID
          , 'name'        , (SELECT name FROM scenario WHERE scenario.id = ScID)
          , 'created'     , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          , 'lastmodified', (SELECT round(EXTRACT(epoch FROM MAX(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          );
END;
$func$ LANGUAGE plpgsql;
