CREATE OR REPLACE FUNCTION delete_scenario(ScID bigint, UserId bigint, AuthRole text)
  RETURNS jsonb AS
$func$
DECLARE
  allowed boolean;
BEGIN
  allowed :=
    (CASE AuthRole
          WHEN 'AuthRoleStudent'   THEN FALSE
          WHEN 'AuthRoleLocal'     THEN (UserId = CAST((SELECT scenario.owner
                                                         FROM scenario
                                                         WHERE scenario.id = ScId)
                                                      AS bigint))
          WHEN 'AuthRoleSuperUser' THEN TRUE
          WHEN NULL                THEN TRUE
          ELSE                          FALSE
     END);

  IF (NOT allowed) THEN
      RETURN NULL;
  END IF;

  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to delete a scenario that does not exist!';
  END IF;

  UPDATE scenario
     SET alive = FALSE
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
