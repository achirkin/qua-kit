-- Create a new scenario.
-- Input geometry is assumed to be in metric system.
-- Optional parameters are longitude/latitute in degrees and altitude in meters
CREATE OR REPLACE FUNCTION create_scenario( scName text
                                          , geom_input jsonb)
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
  -- get time of insertion
  curTime := CURRENT_TIMESTAMP;
  -- FeatureCollection is in 'geometry' property of 'geometry_input' json
  fc := geom_input -> 'geometry';
  IF geom_input ? 'lon' AND jsonb_typeof(geom_input -> 'lon') = 'number' AND
     geom_input ? 'lat' AND jsonb_typeof(geom_input -> 'lat') = 'number'
  THEN
    scLon := CAST((geom_input ->> 'lon') AS decimal);
    scLat := CAST((geom_input ->> 'lat') AS decimal);
  ELSE
    scLon := NULL;
    scLat := NULL;
  END IF;
  IF geom_input ? 'alt' AND jsonb_typeof(geom_input -> 'alt') = 'number'
  THEN
    scAlt := coalesce(CAST((geom_input ->> 'alt') AS decimal), 0);
  ELSE
    scAlt := 0;
  END IF;
  IF geom_input ? 'srid' AND jsonb_typeof(geom_input -> 'srid') = 'number'
  THEN
    scSRID := CAST((geom_input ->> 'srid') AS integer);
  ELSE
    scSRID := NULL;
  END IF;


  IF scSRID IS NOT NULL
     AND EXISTS ( SELECT 1 FROM spatial_ref_sys WHERE srid = scSRID )
  THEN -- if a proper SRID was specified, use it in a new scenario as is.
    SELECT ST_X(p), ST_Y(p), ST_Z(p)
      FROM (SELECT (ST_Transform(ST_SetSRID(ST_MakePoint(0,0,0), scSRID), 4326)) AS p) q
      INTO scLon, scLat, scAlt;
  ELSE -- if a proper SRID was not specified, create a new one.
    -- http://lists.osgeo.org/pipermail/postgis-users/2012-July/034716.html
    -- user-defined srids can be in range from 910000 and below 999000
    SELECT (GREATEST(910000, (SELECT MAX(srid) FROM spatial_ref_sys))+1) INTO scSRID;

    -- add a custom metric reference system with center at specified latitude and longitude
    INSERT INTO spatial_ref_sys (srid, auth_name, auth_srid, srtext, proj4text)
         VALUES ( scSRID
                , scName
                , NULL
                , 'PROJCS["' || scName || '"'
                     || ',GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]]'
                                     || ',PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]]'
                                     || ',UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]]'
                                     || ',AUTHORITY["EPSG","4326"]]'
                            || ',PROJECTION["Lambert_Azimuthal_Equal_Area"]'
                            || ',PARAMETER["latitude_of_center",'  || cast(coalesce(scLat, 0) as text) || ']'
                            || ',PARAMETER["longitude_of_center",' || cast(coalesce(scLon, 0) as text) || ']'
                            || ',PARAMETER["false_easting",0]'
                            || ',PARAMETER["false_northing",0]'
                            || ',UNIT["metre",1,AUTHORITY["EPSG","9001"]]'
                            || ']'
                , '+proj=laea +lat_0=' || cast(coalesce(scLat, 0) as text)
                         || ' +lon_0=' || cast(coalesce(scLon, 0) as text)
                         || ' +ellps=WGS84 +units=m +no_defs'
                );
  END IF;

  -- create a new scenario and keep its id in ScID variable
  INSERT INTO scenario (name, lon, lat, alt, srid) VALUES (scName, scLon, scLat, scAlt, scSRID) RETURNING scenario.id INTO ScID;

  -- insert all scenario properties
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, value)
       SELECT ScID, sprops.key, curTime, sprops.value
         FROM jsonb_each_text(jsonb_strip_nulls(geom_input -> 'properties') ) sprops;
  INSERT INTO scenario_prop (scenario_id, name, last_update)
       SELECT ph.scenario_id, ph.name, ph.ts_update
         FROM scenario_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = ScID;

  -- parse all features
  CREATE TEMP TABLE features ON COMMIT DROP AS
  ( WITH fcs AS (SELECT jsonb_array_elements(fc -> 'features') AS data)
    SELECT CAST(coalesce(nullif(fcs.data -> 'properties' ->> 'geomID',''),nullif(fcs.data ->> 'id','')) AS integer) as geomID
         , ST_Force3D(ST_Transform(ST_SetSRID(ST_GeomFromGeoJSON(fcs.data ->> 'geometry'), scSRID), 4326)) as geom
         , (jsonb_strip_nulls(fcs.data -> 'properties') - 'geomID') AS props
    FROM fcs
  );
  CREATE TEMP SEQUENCE geomID_seq;
  SELECT setval('geomID_seq', max(geomID)) FROM features INTO geomID_max;

  -- fill in null geomIDs
  UPDATE features
     SET geomID = nextval('geomID_seq')
   WHERE geomID IS NULL;
  -- .. and remove temp sequence
  geomID_max := currval('geomID_seq');
  DROP SEQUENCE geomID_seq;

  -- Now, all geomIDs are in place, so I can add new geometries
  INSERT INTO sc_geometry_history (scenario_id, id, ts_update, geom)
       SELECT ScID, features.geomID, curTime, features.geom
         FROM features;
  INSERT INTO sc_geometry (scenario_id, id, last_update)
       SELECT ScID, features.geomID, curTime
         FROM features;
  INSERT INTO sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update, value)
       SELECT ScID, f.geomID, fprops.key, curTime, fprops.value
         FROM features f, jsonb_each_text(f.props) fprops;
  INSERT INTO sc_geometry_prop (scenario_id, geometry_id, name, last_update)
       SELECT ph.scenario_id, ph.geometry_id, ph.name, ph.ts_update
         FROM sc_geometry_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = ScID;

  -- finish!
  RETURN jsonb_build_object
          ( 'ScID'        , ScID
          , 'name'        , scName
          , 'created'     , round(EXTRACT(epoch FROM curTime))
          , 'lastmodified', round(EXTRACT(epoch FROM curTime))
          );
END;
$func$ LANGUAGE plpgsql;
