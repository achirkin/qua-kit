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
