CREATE OR REPLACE FUNCTION get_scenario(ScID bigint)
  RETURNS jsonb AS
$func$
BEGIN
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to get a scenario that has not ever existed!';
  END IF;
  RETURN
    ( SELECT jsonb_build_object(
        'type',     'FeatureCollection',
        'features', jsonb_agg(feature),
        'properties', ( SELECT jsonb_object_agg(ph.name, ph.value)
                          FROM scenario_prop p, scenario_prop_history ph
                         WHERE p.scenario_id = ScID
                           AND ph.scenario_id = ScID
                           AND p.name = ph.name
                           AND ph.alive ),
        'name', (SELECT name FROM SCENARIO WHERE scenario.id = ScID)
        ) as FeatureCollection
      FROM (
        SELECT jsonb_build_object(
          'type',       'Feature',
          'id',         g.id,
          'geometry',   ST_AsGeoJSON(g.geom,15,5)::jsonb,
          'properties', ( SELECT jsonb_object_agg(ph.name, ph.value)
                            FROM sc_geometry_prop p, sc_geometry_prop_history ph
                           WHERE p.scenario_id = ScID
                             AND ph.scenario_id = ScID
                             AND p.geometry_id = g.id
                             AND ph.geometry_id = g.id
                             AND p.name = ph.name
                             AND ph.alive
                        ) || jsonb_build_object('geomID' , g.id)
        ) as feature
        FROM (SELECT gh.*
                FROM sc_geometry, sc_geometry_history gh
               WHERE sc_geometry.last_update = gh.ts_update
                 AND sc_geometry.scenario_id = ScID
                 AND gh.alive
                 AND gh.scenario_id = ScID) g
        ) features
    );
END;
$func$ LANGUAGE plpgsql;
