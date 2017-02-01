CREATE OR REPLACE FUNCTION dropdb() RETURNS void AS
$$
BEGIN
  -- drop all tables in DB
  IF EXISTS
           ( SELECT 1
             FROM information_schema.columns
             WHERE table_name='sc_geometry_history' AND column_name='geom'
           ) THEN
    PERFORM DropGeometryColumn ('sc_geometry_history','geom');
  END IF;
  BEGIN
    ALTER TABLE sc_geometry_prop DROP CONSTRAINT latest_geom_prop;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "latest_geom_prop" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry_prop" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry DROP CONSTRAINT latest_geometry;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "latest_geometry" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop DROP CONSTRAINT latest_sc_prop;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "latest_sc_prop" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "scenario_prop" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_prop_history DROP CONSTRAINT prev_geom_prop_version;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "prev_geom_prop_version" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry_prop_history" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_history DROP CONSTRAINT prev_geometry_version;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "prev_geometry_version" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry_history" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop_history DROP CONSTRAINT prev_sc_prop_version;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "prev_sc_prop_version" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "scenario_prop_history" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_prop DROP CONSTRAINT sc_geometry_properties;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "sc_geometry_properties" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry_prop" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry DROP CONSTRAINT scenario_content;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "scenario_content" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "sc_geometry" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop DROP CONSTRAINT scenario_properies;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "scenario_properies" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "scenario_prop" does not exist, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario DROP CONSTRAINT scenario_srid;
  EXCEPTION
    WHEN undefined_object THEN RAISE NOTICE 'Constraint "scenario_srid" does not exist, skipping.';
    WHEN undefined_table THEN RAISE NOTICE 'Table "scenario" does not exist, skipping.';
  END;

  DROP TABLE IF EXISTS sc_geometry CASCADE;
  DROP TABLE IF EXISTS sc_geometry_history CASCADE;
  DROP TABLE IF EXISTS sc_geometry_prop CASCADE;
  DROP TABLE IF EXISTS sc_geometry_prop_history CASCADE;
  DROP TABLE IF EXISTS scenario CASCADE;
  DROP TABLE IF EXISTS scenario_prop CASCADE;
  DROP TABLE IF EXISTS scenario_prop_history CASCADE;

END;
$$ LANGUAGE plpgsql;
