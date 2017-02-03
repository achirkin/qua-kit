CREATE OR REPLACE FUNCTION update_scenario( ScID bigint
                                          , geom_input jsonb)
  RETURNS jsonb AS
$func$
DECLARE
  curTime TIMESTAMP;
  geomID_max bigint;
  scSRID integer;
  fc jsonb;
BEGIN
  -- get time of insertion
  curTime := CURRENT_TIMESTAMP;
  SELECT scenario.srid
    FROM scenario
   WHERE scenario.id = ScID
    INTO scSRID;

  -- create a new scenario and keep its id in ScID variable
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'Use "create_scenario" function instead';
  END IF;

  -- FeatureCollection is in 'geometry' property of 'geometry_input' json
  fc := geom_input -> 'geometry';

  -- update all scenario properties
  WITH latest_props AS
    ( SELECT ph.* FROM scenario_prop p, scenario_prop_history ph
                 WHERE p.scenario_id = ScID
                   AND ph.scenario_id = ScID
                   AND p.last_update = ph.ts_update
                   AND p.name = ph.name
    )
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, ts_prev_update, alive, value)
       SELECT ScID, sprops.key, curTime, ph.ts_update
            , jsonb_typeof(sprops.value) <> 'null'
            , CASE WHEN jsonb_typeof(sprops.value) = 'null' THEN ph.value
                   ELSE sprops.value #>> '{}'
              END
         FROM jsonb_each(geom_input -> 'properties') sprops
    LEFT JOIN latest_props ph
           ON sprops.key = ph.name AND ScID = ph.scenario_id;
  -- update latest property references
  INSERT INTO scenario_prop (scenario_id, name, last_update)
       SELECT ph.scenario_id, ph.name, ph.ts_update
         FROM scenario_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = ScID
  ON CONFLICT (scenario_id, name) DO UPDATE
          SET last_update = curTime;

  -- parse all features
  CREATE TEMP TABLE features ON COMMIT DROP AS
  ( WITH fcs AS (SELECT jsonb_array_elements(fc -> 'features') AS data)
    SELECT CAST(coalesce(nullif(fcs.data -> 'properties' ->> 'geomID',''),nullif(fcs.data ->> 'id','')) AS integer) as geomID
         , ST_Force3D(ST_Transform(ST_SetSRID(ST_GeomFromGeoJSON(fcs.data ->> 'geometry'), scSRID), 4326)) as geom
         , (jsonb_strip_nulls(fcs.data -> 'properties') - 'geomID') AS props
    FROM fcs
    WHERE fcs.data ? 'geometry' AND jsonb_typeof(fcs.data -> 'geometry') = 'object'
  );
  CREATE TEMP SEQUENCE geomID_seq;
  SELECT setval('geomID_seq'
               , greatest( (SELECT max(geomID) FROM features)
                         , (SELECT max(g.id) FROM sc_geometry_history g WHERE g.scenario_id = ScID)
                         )
               ) INTO geomID_max;

  -- fill in null geomIDs
  UPDATE features
     SET geomID = nextval('geomID_seq')
   where geomID IS NULL;
  -- .. and remove temp sequence
  geomID_max := currval('geomID_seq');
  DROP SEQUENCE geomID_seq;

  -- Create a temporary table with all ids of features to delete
  CREATE TEMP TABLE todelete ON COMMIT DROP AS
  ( WITH fcs AS (SELECT jsonb_array_elements(fc -> 'features') AS data)
    SELECT DISTINCT CAST(ids.value AS bigint) as id
    FROM fcs fc, jsonb_array_elements_text(fc.data #> '{properties,deleted_geomIDs}') ids
  );

  -- Now, all geomIDs are in place, so I can add new geometries
  -- update all geometries in history table
  INSERT INTO sc_geometry_history (scenario_id, id, ts_update, ts_prev_update, alive, geom)
       SELECT ScID, features.geomID, curTime, sc_geometry.last_update, TRUE, features.geom
         FROM features
    LEFT JOIN sc_geometry
           ON features.geomID = sc_geometry.id
          AND ScID = sc_geometry.scenario_id;
  -- update geometry references
  INSERT INTO sc_geometry (scenario_id, id, last_update)
       SELECT ScID, gh.id, curTime
         FROM sc_geometry_history gh
        WHERE gh.ts_update = curTime AND gh.scenario_id = ScID
  ON CONFLICT (scenario_id, id) DO UPDATE
          SET last_update = curTime;


  -- update all feature properties
  WITH latest_props AS
    ( SELECT ph.* FROM sc_geometry_prop p, sc_geometry_prop_history ph
                 WHERE p.scenario_id = ScID
                   AND ph.scenario_id = ScID
                   AND p.geometry_id = ph.geometry_id
                   AND p.last_update = ph.ts_update
                   AND p.name = ph.name
    )
  INSERT INTO sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update, ts_prev_update, alive, value)
       SELECT ScID, fprops.geomID, fprops.key, curTime, ph.ts_update
            , jsonb_typeof(fprops.value) <> 'null'
            , CASE WHEN jsonb_typeof(fprops.value) = 'null' THEN ph.value
                   ELSE fprops.value #>> '{}'
              END
         FROM (SELECT f.geomID, ps.key, ps.value FROM features f, jsonb_each(f.props) ps) fprops
    LEFT JOIN latest_props ph
           ON fprops.key = ph.name AND ScID = ph.scenario_id AND fprops.geomID = ph.geometry_id;
  -- update latest property references
  INSERT INTO sc_geometry_prop (scenario_id, geometry_id, name, last_update)
       SELECT ph.scenario_id, ph.geometry_id, ph.name, curTime
         FROM sc_geometry_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = ScID
  ON CONFLICT (scenario_id, geometry_id, name) DO UPDATE
          SET last_update = curTime;

  -- mark geometries as not alive
  UPDATE sc_geometry_history
     SET alive = FALSE
    FROM sc_geometry, todelete
   WHERE sc_geometry.id = todelete.id
     AND sc_geometry.id = sc_geometry_history.id
     AND sc_geometry.scenario_id = ScID
     AND sc_geometry.scenario_id = sc_geometry_history.scenario_id
     AND sc_geometry.last_update = sc_geometry_history.ts_update;

  -- finish!
  RETURN jsonb_build_object
          ( 'ScID'        , ScID
          , 'name'        , (SELECT name FROM scenario WHERE scenario.id = ScID)
          , 'created'     , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
          , 'lastmodified', round(EXTRACT(epoch FROM curTime))
          );
END;
$func$ LANGUAGE plpgsql;
