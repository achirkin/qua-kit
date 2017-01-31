-- I think it is a good idea to conform to the newest GeoJSON specifiation;
-- https://tools.ietf.org/html/rfc7946
-- This means I should use World Geodetic System 1984 (WGS 84) (long-lat in decimal degrees)
-- I can find it under following URN:
--             urn:ogc:def:crs:OGC::CRS84
-- It also has followind SRID in PostGIS: 4326
-- To transform a geometry into this CRS, one can use following function:
--                                                 ST_Transform( geom, 4326 )

-- This query does:
--   1) Specify a boundary in desired CRS (ST_MakeEnvelope)
--   2) Transform the boundary to a CRS of data table
--   3) Select polygons within this boundary (only 3 of them)
--   4) Put all non-null properties into Feature together with geometry
--   5) Pack all found features in a single FeatureCollection object

SELECT jsonb_build_object(
    'type',     'FeatureCollection',
    'features', jsonb_agg(feature)
) as FeatureCollection
FROM (
  SELECT jsonb_build_object(
    'type',       'Feature',
    'id',         p.osm_id,
    'geometry',   ST_AsGeoJSON(p.way,15,5)::jsonb,
    'properties', jsonb_strip_nulls(to_jsonb(p)) - 'way' - 'osm_id'
  ) as feature FROM (SELECT *
          FROM planet_osm_polygon
          WHERE planet_osm_polygon.way &&
                  ST_Transform( ST_MakeEnvelope(8.538, 47.367, 8.540, 47.368, 4326)
                              , 900913)
          LIMIT 3) p) features;




-- create user and database

CREATE USER siren WITH PASSWORD 'sirenpass';
CREATE DATABASE sirendb;
GRANT ALL PRIVILEGES ON DATABASE sirendb to siren;



-- set up database

CREATE EXTENSION postgis;
CREATE EXTENSION postgis_topology;
CREATE EXTENSION hstore;



-- create all tables in DB

CREATE TABLE sc_geometry (scenario_id bigint NOT NULL, id bigint NOT NULL, last_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, PRIMARY KEY (scenario_id, id));
CREATE TABLE sc_geometry_history (scenario_id bigint NOT NULL, id bigint NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (scenario_id, id, ts_update));
SELECT AddGeometryColumn ('sc_geometry_history','geom',4326,'Geometry',3);
CREATE TABLE sc_geometry_prop (scenario_id bigint NOT NULL, geometry_id bigint NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name));
CREATE TABLE sc_geometry_prop_history (scenario_id bigint NOT NULL, geometry_id bigint NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp DEFAULT NULL, alive bool DEFAULT 'TRUE' NOT NULL, value text NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name, ts_update));
CREATE TABLE scenario (id bigserial, alive bool DEFAULT 'TRUE' NOT NULL, name text NOT NULL, PRIMARY KEY (id));
CREATE TABLE scenario_prop (scenario_id bigint NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, PRIMARY KEY (scenario_id, name));
CREATE TABLE scenario_prop_history (scenario_id bigint NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp DEFAULT NULL, alive bool DEFAULT 'TRUE' NOT NULL, value text NOT NULL, PRIMARY KEY (scenario_id, name, ts_update));
CREATE INDEX sc_geometry_scenario_id ON sc_geometry (scenario_id);
CREATE INDEX sc_geometry_id ON sc_geometry (id);
CREATE INDEX sc_geometry_history_scenario_id ON sc_geometry_history (scenario_id);
CREATE INDEX sc_geometry_history_id ON sc_geometry_history (id);
CREATE INDEX sc_geometry_history_ts_update ON sc_geometry_history (ts_update);
CREATE INDEX sc_geometry_prop_scenario_id ON sc_geometry_prop (scenario_id);
CREATE INDEX sc_geometry_prop_geometry_id ON sc_geometry_prop (geometry_id);
CREATE INDEX sc_geometry_prop_name ON sc_geometry_prop (name);
CREATE INDEX sc_geometry_prop_history_scenario_id ON sc_geometry_prop_history (scenario_id);
CREATE INDEX sc_geometry_prop_history_geometry_id ON sc_geometry_prop_history (geometry_id);
CREATE INDEX sc_geometry_prop_history_name ON sc_geometry_prop_history (name);
CREATE INDEX sc_geometry_prop_history_ts_update ON sc_geometry_prop_history (ts_update);
CREATE UNIQUE INDEX scenario_id ON scenario (id);
CREATE INDEX scenario_prop_scenario_id ON scenario_prop (scenario_id);
CREATE INDEX scenario_prop_name ON scenario_prop (name);
CREATE INDEX scenario_prop_history_scenario_id ON scenario_prop_history (scenario_id);
CREATE INDEX scenario_prop_history_name ON scenario_prop_history (name);
CREATE INDEX scenario_prop_history_ts_update ON scenario_prop_history (ts_update);
ALTER TABLE sc_geometry_prop ADD CONSTRAINT latest_geom_prop FOREIGN KEY (scenario_id, geometry_id, name, last_update) REFERENCES sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update);
ALTER TABLE sc_geometry ADD CONSTRAINT latest_geometry FOREIGN KEY (scenario_id, id, last_update) REFERENCES sc_geometry_history (scenario_id, id, ts_update);
ALTER TABLE scenario_prop ADD CONSTRAINT latest_sc_prop FOREIGN KEY (scenario_id, name, last_update) REFERENCES scenario_prop_history (scenario_id, name, ts_update);
ALTER TABLE sc_geometry_prop_history ADD CONSTRAINT prev_geom_prop_version FOREIGN KEY (scenario_id, geometry_id, name, ts_prev_update) REFERENCES sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update);
ALTER TABLE sc_geometry_history ADD CONSTRAINT prev_geometry_version FOREIGN KEY (scenario_id, id, ts_prev_update) REFERENCES sc_geometry_history (scenario_id, id, ts_update);
ALTER TABLE scenario_prop_history ADD CONSTRAINT prev_sc_prop_version FOREIGN KEY (scenario_id, name, ts_prev_update) REFERENCES scenario_prop_history (scenario_id, name, ts_update);
ALTER TABLE sc_geometry_prop ADD CONSTRAINT sc_geometry_properties FOREIGN KEY (scenario_id, geometry_id) REFERENCES sc_geometry (scenario_id, id);
ALTER TABLE sc_geometry ADD CONSTRAINT scenario_content FOREIGN KEY (scenario_id) REFERENCES scenario (id);
ALTER TABLE scenario_prop ADD CONSTRAINT scenario_properies FOREIGN KEY (scenario_id) REFERENCES scenario (id);


-- drop all tables in DB

SELECT DropGeometryColumn ('sc_geometry_history','geom');
ALTER TABLE sc_geometry_prop DROP CONSTRAINT latest_geom_prop;
ALTER TABLE sc_geometry DROP CONSTRAINT latest_geometry;
ALTER TABLE scenario_prop DROP CONSTRAINT latest_sc_prop;
ALTER TABLE sc_geometry_prop_history DROP CONSTRAINT prev_geom_prop_version;
ALTER TABLE sc_geometry_history DROP CONSTRAINT prev_geometry_version;
ALTER TABLE scenario_prop_history DROP CONSTRAINT prev_sc_prop_version;
ALTER TABLE sc_geometry_prop DROP CONSTRAINT sc_geometry_properties;
ALTER TABLE sc_geometry DROP CONSTRAINT scenario_content;
ALTER TABLE scenario_prop DROP CONSTRAINT scenario_properies;
DROP TABLE IF EXISTS sc_geometry CASCADE;
DROP TABLE IF EXISTS sc_geometry_history CASCADE;
DROP TABLE IF EXISTS sc_geometry_prop CASCADE;
DROP TABLE IF EXISTS sc_geometry_prop_history CASCADE;
DROP TABLE IF EXISTS scenario CASCADE;
DROP TABLE IF EXISTS scenario_prop CASCADE;
DROP TABLE IF EXISTS scenario_prop_history CASCADE;


-- create a new scenario

CREATE OR REPLACE FUNCTION create_scenario(scName text, fc jsonb)
  RETURNS bigint AS
$func$
DECLARE
  ScID bigint;
  curTime TIMESTAMP;
  geomID_max bigint;
BEGIN
  -- get time of insertion
  curTime := CURRENT_TIMESTAMP;

  -- create a new scenario and keep its id in ScID variable
  INSERT INTO scenario (name) VALUES (scName) RETURNING scenario.id INTO ScID;

  -- insert all scenario properties
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, value)
       SELECT ScID, sprops.key, curTime, sprops.value
         FROM jsonb_each_text(jsonb_strip_nulls(fc -> 'properties') ) sprops;
  INSERT INTO scenario_prop (scenario_id, name, last_update)
       SELECT ph.scenario_id, ph.name, ph.ts_update
         FROM scenario_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = ScID;

  -- parse all features
  CREATE TEMP TABLE features ON COMMIT DROP AS
  ( WITH fcs AS (SELECT jsonb_array_elements(fc -> 'features') AS data)
    SELECT CAST(coalesce(nullif(fcs.data -> 'properties' ->> 'geomID',''),nullif(fcs.data ->> 'id','')) AS integer) as geomID
         , ST_Force3D(ST_SetSRID(ST_GeomFromGeoJSON(fcs.data ->> 'geometry'), 4326)) as geom
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
  RETURN ScID;
END;
$func$ LANGUAGE plpgsql;


-- update an existing scenario

CREATE OR REPLACE FUNCTION update_scenario(ScID bigint, fc jsonb)
  RETURNS bigint AS
$func$
DECLARE
  curTime TIMESTAMP;
  geomID_max bigint;
BEGIN
  -- get time of insertion
  curTime := CURRENT_TIMESTAMP;

  -- create a new scenario and keep its id in ScID variable
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'Use "create_scenario" function instead';
  END IF;

  -- update all scenario properties
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, ts_prev_update, alive, value)
       SELECT ScID, sprops.key, curTime, ph.ts_update
            , jsonb_typeof(sprops.value) <> 'null'
            , CASE WHEN jsonb_typeof(sprops.value) = 'null' THEN ph.value
                   ELSE sprops.value #>> '{}'
              END
         FROM jsonb_each(fc -> 'properties') sprops
    LEFT JOIN ( SELECT scenario_prop_history.*
                  FROM scenario_prop, scenario_prop_history
                 WHERE scenario_prop.name = scenario_prop_history.name
                   AND scenario_prop.scenario_id = ScID
                   AND scenario_prop_history.scenario_id = ScID
                   AND scenario_prop.last_update = scenario_prop_history.ts_update
              ) ph
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
         , ST_Force3D(ST_SetSRID(ST_GeomFromGeoJSON(fcs.data ->> 'geometry'), 4326)) as geom
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
  INSERT INTO sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update, ts_prev_update, alive, value)
       SELECT ScID, fprops.geomID, fprops.key, curTime, ph.ts_update
            , jsonb_typeof(fprops.value) <> 'null'
            , CASE WHEN jsonb_typeof(fprops.value) = 'null' THEN ph.value
                   ELSE fprops.value #>> '{}'
              END
         FROM (SELECT f.geomID, ps.key, ps.value FROM features f, jsonb_each(f.props) ps) fprops
    LEFT JOIN sc_geometry_prop_history ph
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
  RETURN ScID;
END;
$func$ LANGUAGE plpgsql;


-- mark scenario as dead

CREATE OR REPLACE FUNCTION delete_scenario(ScID bigint)
  RETURNS bigint AS
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
  RETURN ScID;
END;
$func$ LANGUAGE plpgsql;


-- mark scenario as alive

CREATE OR REPLACE FUNCTION recover_scenario(ScID bigint)
  RETURNS bigint AS
$func$
BEGIN
  IF (SELECT count(*) < 1 FROM scenario WHERE scenario.id = ScID) THEN
    RAISE EXCEPTION 'Nonexistent scenario (ScID = %)', ScID
          USING HINT = 'You are trying to recover a scenario that has not ever existed!';
  END IF;
  UPDATE scenario
     SET alive = TRUE
   WHERE scenario.id = ScID;
  -- finish!
  RETURN ScID;
END;
$func$ LANGUAGE plpgsql;


-- get latest state of a scenario

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

-- list alive scenarios

CREATE OR REPLACE FUNCTION list_scenarios()
  RETURNS jsonb AS
$func$
BEGIN
  RETURN
    ( SELECT jsonb_build_object(
        'scenarios',
          ( SELECT jsonb_agg
            ( jsonb_build_object
              ( 'ScID',         scenario.id
              , 'name',         scenario.name
              , 'created',      (SELECT EXTRACT(epoch FROM MIN(ts_update)) FROM sc_geometry_history WHERE scenario_id = scenario.id)
              , 'lastmodified', (SELECT EXTRACT(epoch FROM MAX(ts_update)) FROM sc_geometry_history WHERE scenario_id = scenario.id)
              )
            )
            FROM scenario
            WHERE scenario.alive
          )
        )
    );
END;
$func$ LANGUAGE plpgsql;

-- test query!
SELECT create_scenario('Great Scenario', '{"type": "FeatureCollection", "features": [{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"geomID": 32233957, "z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"id": 12, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}}, {"id": 5, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [949748.439999999944121, 6002121.940000000409782, 950896.020000000018626, 6003940.440000000409782], "type": "Polygon", "coordinates": [[[949748.44, 6003014.51], [949833.69, 6003199.1], [949897.44, 6003342.2], [950004.94, 6003503.8], [950082.04, 6003609.07], [950193.97, 6003536.41], [950225.86, 6003652.07], [950253.28, 6003642.42], [950297.77, 6003741.76], [950446.03, 6003940.44], [950509.05, 6003915.24], [950492.73, 6003870.02], [950739.61, 6003775.13], [950752.21, 6003809.23], [950827.08, 6003809.97], [950861.18, 6003655.03], [950870.82, 6003594.98], [950853.76, 6003483.78], [950737.38, 6003499.34], [950705.5, 6003475.62], [950701.79, 6003402.24], [950875.27, 6003397.05], [950868.59, 6003365.92], [950889.35, 6003286.59], [950870.82, 6003199.1], [950870.07, 6003126.47], [950867.86, 6003026.38], [950874.52, 6003027.86], [950880.46, 6002755.79], [950871.56, 6002653.49], [950896.02, 6002653.49], [950885.65, 6002557.86], [950857.47, 6002253.91], [950818.92, 6002250.21], [950809.29, 6002300.62], [950658.8, 6002273.19], [950674.36, 6002222.04], [950635.08, 6002204.98], [950661.03, 6002157.53], [950603.94, 6002121.94], [950403.78, 6002451.85], [950363.75, 6002498.54], [950320.74, 6002518.56], [950165.81, 6002531.91], [950078.33, 6002724.66], [950153.21, 6002802.49], [950113.18, 6002834.38], [950025.69, 6002971.53], [949960.4, 6002990.94], [949966.39, 6003018.23], [949839.62, 6003084.21], [949784.02, 6002988.57], [949748.44, 6003014.51]]]}, "properties": {"landuse": "residential", "z_order": 0, "way_area": 1191190}}], "properties" : {"ScenarioProp1": 162, "asgas": null, "Hello" : "World"}}');


SELECT update_scenario(17, '{"type": "FeatureCollection", "features": [{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"geomID": 32233957, "z_order": 0, "building": "yes", "way_area": 3149.87, "deleted_geomIDs": [182,2]}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"id": 12, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}}, {"id": 5, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [949748.439999999944121, 6002121.940000000409782, 950896.020000000018626, 6003940.440000000409782], "type": "Polygon", "coordinates": [[[949748.44, 6003014.51], [949833.69, 6003199.1], [949897.44, 6003342.2], [950004.94, 6003503.8], [950082.04, 6003609.07], [950193.97, 6003536.41], [950225.86, 6003652.07], [950253.28, 6003642.42], [950297.77, 6003741.76], [950446.03, 6003940.44], [950509.05, 6003915.24], [949784.02, 6002988.57], [949748.44, 6003014.51]]]}, "properties": {"landuse": "residential", "z_order": 0, "way_area": 1191190}},{"type": "Feature", "properties": {"deleted_geomIDs": [18,232,12]}}], "properties" : {"ScenarioProp1": null, "asgas": 42, "Hello" : "Nothing"}}');


SELECT update_scenario(2, '{"type": "FeatureCollection", "features": [{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"geomID": 32233957, "z_order": 0, "building": "yes", "way_area": 3149.87, "deleted_geomIDs": [182,2]}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"id": 12, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}}, {"id": 5, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [949748.439999999944121, 6002121.940000000409782, 950896.020000000018626, 6003940.440000000409782], "type": "Polygon", "coordinates": [[[949748.44, 6003014.51], [949833.69, 6003199.1], [949897.44, 6003342.2], [950004.94, 6003503.8], [950082.04, 6003609.07], [950193.97, 6003536.41], [950225.86, 6003652.07], [950253.28, 6003642.42], [950297.77, 6003741.76], [950446.03, 6003940.44], [950509.05, 6003915.24], [949784.02, 6002988.57], [949748.44, 6003014.51]]]}, "properties": {"landuse": "residential", "z_order": 0, "way_area": 1191190}},{"type": "Feature", "properties": {"deleted_geomIDs": [18,232,12]}}], "properties" : {"ScenarioProp1": null, "asgas": 42, "Hello" : "Nothing"}}');

