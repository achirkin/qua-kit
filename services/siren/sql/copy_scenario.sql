-- Copy an existing scenario.
-- We copy only latest snapshots of all properties and omit deleted properties.
CREATE OR REPLACE FUNCTION copy_scenario( userId bigint
                                        , authRole text
                                        , oldScID bigint)
  RETURNS jsonb AS
$func$
DECLARE
  ScID bigint;
  curTime TIMESTAMP;
  geomID_max bigint;
  scSRID integer;
  fc jsonb;
  scLon decimal;
  scLat decimal;
  scAlt decimal;
BEGIN

  IF (CASE authRole
          WHEN 'Student' THEN FALSE
          WHEN 'Local'   THEN FALSE
          WHEN 'Admin'   THEN FALSE
          ELSE                TRUE
      END)
  THEN
    RAISE EXCEPTION 'You do not have rights to copy scenarios.'
          USING HINT = 'Actually, you should have; but something went wrong...';
  END IF;

  -- copy a scenario and keep its id in ScID variable
  INSERT INTO scenario (name, owner, lon, lat, alt, srid)
       SELECT o.name, userId, o.lon, o.lat, o.alt, o.srid
         FROM scenario o
        WHERE o.id = oldScID
    RETURNING scenario.id INTO ScID;

  -- insert all scenario properties
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, value)
       SELECT ScID, sph.name, sph.ts_update, sph.value
         FROM scenario_prop_history sph, scenario_prop sp
        WHERE sp.last_update = sph.ts_update
          AND sp.name = sph.name
          AND sp.scenario_id = oldScID
          AND sph.scenario_id = oldScID
          AND sph.alive;
  -- by now all the properties for a new scenario are good, distinct, and alive
  INSERT INTO scenario_prop (scenario_id, name, last_update)
       SELECT ph.scenario_id, ph.name, ph.ts_update
         FROM scenario_prop_history ph
        WHERE ph.scenario_id = ScID;


  -- add geometries
  INSERT INTO sc_geometry_history (scenario_id, id, ts_update, geom)
       SELECT ScID, gh.id, gh.ts_update, gh.geom
         FROM sc_geometry_history gh, sc_geometry g
        WHERE g.last_update = gh.ts_update
          AND g.id = gh.id
          AND g.scenario_id = oldScID
          AND gh.scenario_id = oldScID
          AND gh.alive;
  INSERT INTO sc_geometry (scenario_id, id, last_update)
       SELECT gh.scenario_id, gh.id, gh.ts_update
         FROM sc_geometry_history gh
        WHERE gh.scenario_id = ScID;

  -- add geometry properties
  INSERT INTO sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update, value)
       SELECT ScID, gph.geometry_id, gph.name, gph.ts_update, gph.value
         FROM sc_geometry_prop_history gph, sc_geometry_prop gp
        WHERE gp.last_update = gph.ts_update
          AND gp.name = gph.name
          AND gp.geometry_id = gph.geometry_id
          AND gp.scenario_id = oldScID
          AND gph.scenario_id = oldScID
          AND gph.alive;
  INSERT INTO sc_geometry_prop (scenario_id, geometry_id, name, last_update)
       SELECT gph.scenario_id, gph.geometry_id, gph.name, gph.ts_update
         FROM sc_geometry_prop_history gph
        WHERE gph.scenario_id = ScID;

  -- finish!
  RETURN jsonb_build_object
          ( 'ScID'        , ScID
          , 'name'        , (SELECT name FROM scenario WHERE id = ScID)
          , 'created'     , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          , 'lastmodified', (SELECT round(EXTRACT(epoch FROM MAX(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          );
END;
$func$ LANGUAGE plpgsql;
