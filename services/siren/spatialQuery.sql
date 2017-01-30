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

CREATE TABLE sc_geometry (scenario_id integer NOT NULL, id integer NOT NULL, last_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (scenario_id, id));
CREATE TABLE sc_geometry_history (scenario_id integer NOT NULL, id integer NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_next_update timestamp, PRIMARY KEY (scenario_id, id, ts_update));
SELECT AddGeometryColumn ('sc_geometry_history','geom',4326,'Geometry',3);
CREATE TABLE sc_geometry_prop (scenario_id integer NOT NULL, geometry_id integer NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name));
CREATE TABLE sc_geometry_prop_history (scenario_id integer NOT NULL, geometry_id integer NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_next_update timestamp DEFAULT NULL, value text NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name, ts_update));
CREATE TABLE scenario (id SERIAL, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (id));
CREATE TABLE scenario_prop (scenario_id integer NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (scenario_id, name));
CREATE TABLE scenario_prop_history (scenario_id integer NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_next_update timestamp DEFAULT NULL, value text NOT NULL, PRIMARY KEY (scenario_id, name, ts_update));
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
ALTER TABLE sc_geometry_prop_history ADD CONSTRAINT next_geom_prop_version FOREIGN KEY (scenario_id, geometry_id, name, ts_next_update) REFERENCES sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update);
ALTER TABLE sc_geometry_history ADD CONSTRAINT next_geometry_version FOREIGN KEY (scenario_id, id, ts_next_update) REFERENCES sc_geometry_history (scenario_id, id, ts_update);
ALTER TABLE scenario_prop_history ADD CONSTRAINT next_sc_prop_version FOREIGN KEY (scenario_id, name, ts_next_update) REFERENCES scenario_prop_history (scenario_id, name, ts_update);
ALTER TABLE sc_geometry_prop ADD CONSTRAINT sc_geometry_properties FOREIGN KEY (scenario_id, geometry_id) REFERENCES sc_geometry (scenario_id, id);
ALTER TABLE sc_geometry ADD CONSTRAINT scenario_content FOREIGN KEY (scenario_id) REFERENCES scenario (id);
ALTER TABLE scenario_prop ADD CONSTRAINT scenario_properies FOREIGN KEY (scenario_id) REFERENCES scenario (id);



-- drop all tables in DB

SELECT DropGeometryColumn ('sc_geometry_history','geom');
ALTER TABLE sc_geometry_prop DROP CONSTRAINT latest_geom_prop;
ALTER TABLE sc_geometry DROP CONSTRAINT latest_geometry;
ALTER TABLE scenario_prop DROP CONSTRAINT latest_sc_prop;
ALTER TABLE sc_geometry_prop_history DROP CONSTRAINT next_geom_prop_version;
ALTER TABLE sc_geometry_history DROP CONSTRAINT next_geometry_version;
ALTER TABLE scenario_prop_history DROP CONSTRAINT next_sc_prop_version;
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

CREATE OR REPLACE FUNCTION create_scenario(fc jsonb)
  RETURNS integer AS
$func$
DECLARE
  scID integer;
  curTime TIMESTAMP;
  geomID_max integer;
BEGIN
  -- get time of insertion
  curTime := CURRENT_TIMESTAMP;

  -- create a new scenario and keep its id in scID variable
  INSERT INTO scenario DEFAULT VALUES RETURNING scenario.id INTO scID;

  -- insert all scenario properties
  INSERT INTO scenario_prop_history (scenario_id, name, ts_update, value)
       SELECT scID, sprops.key, curTime, sprops.value
         FROM jsonb_each_text(jsonb_strip_nulls(fc -> 'properties') ) sprops;
  INSERT INTO scenario_prop (scenario_id, name, last_update)
       SELECT ph.scenario_id, ph.name, ph.ts_update
         FROM scenario_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = scID;

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
   where geomID IS NULL;
  -- .. and remove temp sequence
  geomID_max := currval('geomID_seq');
  DROP SEQUENCE geomID_seq;

  -- Now, all geomIDs are in place, so I can add new geometries
  INSERT INTO sc_geometry_history (scenario_id, id, ts_update, geom)
       SELECT scID, features.geomID, curTime, features.geom
         FROM features;
  INSERT INTO sc_geometry (scenario_id, id, last_update)
       SELECT scID, features.geomID, curTime
         FROM features;
  INSERT INTO sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update, value)
       SELECT scID, f.geomID, fprops.key, curTime, fprops.value
         FROM features f, jsonb_each_text(f.props) fprops;
  INSERT INTO sc_geometry_prop (scenario_id, geometry_id, name, last_update)
       SELECT ph.scenario_id, ph.geometry_id, ph.name, ph.ts_update
         FROM sc_geometry_prop_history ph
        WHERE ph.ts_update = curTime AND ph.scenario_id = scID;

  -- finish!
  RETURN scID;
END;
$func$ LANGUAGE plpgsql;



-- test query!
SELECT create_scenario('{"type": "FeatureCollection", "features": [{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"geomID": 32233957, "z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"id": 12, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}},{"type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [950595.989999999990687, 6002245.769999999552965, 950664.880000000004657, 6002318.040000000037253], "type": "Polygon", "coordinates": [[[950595.99, 6002292.88], [950596.07, 6002295.83], [950596.43, 6002299.15], [950598.88, 6002302.67], [950624.2, 6002310.17], [950649.27, 6002318.04], [950656.98, 6002287.09], [950664.88, 6002256.12], [950625.48, 6002245.77], [950595.99, 6002292.88]]]}, "properties": {"z_order": 0, "building": "yes", "way_area": 3149.87}}, {"id": 5, "type": "Feature", "geometry": {"crs": {"type": "name", "properties": {"name": "urn:ogc:def:crs:spatialreferencing.org::900913"}}, "bbox": [949748.439999999944121, 6002121.940000000409782, 950896.020000000018626, 6003940.440000000409782], "type": "Polygon", "coordinates": [[[949748.44, 6003014.51], [949833.69, 6003199.1], [949897.44, 6003342.2], [950004.94, 6003503.8], [950082.04, 6003609.07], [950193.97, 6003536.41], [950225.86, 6003652.07], [950253.28, 6003642.42], [950297.77, 6003741.76], [950446.03, 6003940.44], [950509.05, 6003915.24], [950492.73, 6003870.02], [950739.61, 6003775.13], [950752.21, 6003809.23], [950827.08, 6003809.97], [950861.18, 6003655.03], [950870.82, 6003594.98], [950853.76, 6003483.78], [950737.38, 6003499.34], [950705.5, 6003475.62], [950701.79, 6003402.24], [950875.27, 6003397.05], [950868.59, 6003365.92], [950889.35, 6003286.59], [950870.82, 6003199.1], [950870.07, 6003126.47], [950867.86, 6003026.38], [950874.52, 6003027.86], [950880.46, 6002755.79], [950871.56, 6002653.49], [950896.02, 6002653.49], [950885.65, 6002557.86], [950857.47, 6002253.91], [950818.92, 6002250.21], [950809.29, 6002300.62], [950658.8, 6002273.19], [950674.36, 6002222.04], [950635.08, 6002204.98], [950661.03, 6002157.53], [950603.94, 6002121.94], [950403.78, 6002451.85], [950363.75, 6002498.54], [950320.74, 6002518.56], [950165.81, 6002531.91], [950078.33, 6002724.66], [950153.21, 6002802.49], [950113.18, 6002834.38], [950025.69, 6002971.53], [949960.4, 6002990.94], [949966.39, 6003018.23], [949839.62, 6003084.21], [949784.02, 6002988.57], [949748.44, 6003014.51]]]}, "properties": {"landuse": "residential", "z_order": 0, "way_area": 1191190}}], "properties" : {"ScenarioProp1": 162, "asgas": null, "Hello" : "World"}}');

