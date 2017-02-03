CREATE OR REPLACE FUNCTION get_last_sc_update(ScID bigint)
  RETURNS jsonb AS
  -- TODO: add feature with deleted geom ids
$func$
DECLARE
  features jsonb;
  geometry_output jsonb;
  lastTime TIMESTAMP;
BEGIN
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to get a scenario that has not ever existed!';
  END IF;

  -- get the time of the last scenario update
  lastTime := greatest
    ( (SELECT max(last_update) FROM sc_geometry WHERE scenario_id = ScID)
    , (SELECT max(last_update) FROM sc_geometry_prop WHERE scenario_id = ScID)
    , (SELECT max(last_update) FROM scenario_prop WHERE scenario_id = ScID)
    );

  SELECT jsonb_build_object(
        'type',     'FeatureCollection',
        'features', jsonb_agg(fcs.feature)
        ) as FeatureCollection
    FROM (
        SELECT jsonb_build_object(
          'type',       'Feature',
          'id',         CAST(g.id AS text),
          'geometry',   ST_AsGeoJSON(ST_Transform(g.geom, (SELECT srid FROM scenario WHERE id = ScID)))::jsonb,
          'properties', coalesce(
                        ( SELECT jsonb_object_agg(ph.name, ph.value)
                            FROM sc_geometry_prop_history ph
                           WHERE ph.ts_update = lastTime
                             AND ph.scenario_id = ScID
                             AND ph.geometry_id = g.id
                             AND ph.alive
                        ), '{}') || jsonb_build_object('geomID' , g.id)
        ) as feature
        FROM (SELECT gh.*
                FROM sc_geometry_history gh
               WHERE gh.ts_update = lastTime
                 AND gh.scenario_id = ScID
                 AND gh.alive) g
        WHERE g.geom IS NOT NULL
        ) fcs
    INTO features;

  geometry_output := jsonb_build_object
    ( 'format'       , 'GeoJSON'
    , 'name'         , (SELECT name FROM scenario WHERE id = ScID)
    , 'geometry'     , features
    , 'properties'   , ( SELECT jsonb_object_agg(ph.name, ph.value)
                           FROM scenario_prop_history ph
                          WHERE ph.ts_update = lastTime
                            AND ph.scenario_id = ScID
                            AND ph.alive )
    );

  RETURN jsonb_build_object
    ( 'geometry_output' , geometry_output
    , 'created'         , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
    , 'lastmodified'    , round(EXTRACT(epoch FROM lastTime))
    );
END;
$func$ LANGUAGE plpgsql;
