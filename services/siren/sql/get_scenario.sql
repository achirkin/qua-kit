CREATE OR REPLACE FUNCTION get_scenario(ScID bigint, forceSRID integer)
  RETURNS jsonb AS
$func$
DECLARE
  features jsonb;
  geometry_output jsonb;
  result jsonb;
  scSRID integer;
BEGIN
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to get a scenario that has not ever existed!';
  END IF;

  -- set forced export SRID if necessary.
  SELECT COALESCE(forceSRID, s.srid) FROM scenario s WHERE s.id = ScID INTO scSRID;

  SELECT jsonb_build_object(
        'type',     'FeatureCollection',
        'features', jsonb_agg(fcs.feature)
        ) as FeatureCollection
    FROM (
        SELECT jsonb_build_object(
          'type',       'Feature',
          'id',         CAST(g.id AS text),
          'geometry',   ST_AsGeoJSON(ST_Transform(g.geom, scSRID))::jsonb,
          'properties', coalesce(
                        ( SELECT jsonb_object_agg(ph.name, ph.value)
                            FROM sc_geometry_prop p, sc_geometry_prop_history ph
                           WHERE p.last_update = ph.ts_update
                             AND p.scenario_id = ScID
                             AND ph.scenario_id = ScID
                             AND p.geometry_id = g.id
                             AND ph.geometry_id = g.id
                             AND p.name = ph.name
                             AND ph.alive
                        ), '{}') || jsonb_build_object('geomID' , g.id)
        ) as feature
        FROM (SELECT gh.*
                FROM sc_geometry, sc_geometry_history gh
               WHERE sc_geometry.last_update = gh.ts_update
                 AND sc_geometry.scenario_id = ScID
                 AND gh.scenario_id = ScID
                 AND gh.id = sc_geometry.id
                 AND gh.alive) g
        WHERE g.geom IS NOT NULL
        ) fcs
    INTO features;

  geometry_output := jsonb_build_object
    ( 'format'       , 'GeoJSON'
    , 'name'         , (SELECT name FROM scenario WHERE id = ScID)
    , 'ScID'         , ScID
    , 'srid'         , scSRID
    , 'lon'          , (SELECT lon  FROM scenario WHERE id = ScID)
    , 'lat'          , (SELECT lat  FROM scenario WHERE id = ScID)
    , 'alt'          , (SELECT alt  FROM scenario WHERE id = ScID)
    , 'geometry'     , features
    , 'properties'   , ( SELECT jsonb_object_agg(ph.name, ph.value)
                           FROM scenario_prop p, scenario_prop_history ph
                          WHERE p.last_update = ph.ts_update
                            AND p.scenario_id = ScID
                            AND ph.scenario_id = ScID
                            AND p.name = ph.name
                            AND ph.alive )
    );

  RETURN jsonb_build_object
    ( 'geometry_output' , geometry_output
    , 'ScID'            , ScID
    , 'created'         , (SELECT round(EXTRACT(epoch FROM MIN(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
    , 'lastmodified'    , (SELECT round(EXTRACT(epoch FROM MAX(ts_update))) FROM sc_geometry_history WHERE scenario_id = ScID)
    );
END;
$func$ LANGUAGE plpgsql;
