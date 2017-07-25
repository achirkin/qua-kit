CREATE OR REPLACE FUNCTION populatedb() RETURNS void AS
$$
BEGIN
  -- create all tables in DB
  CREATE TABLE IF NOT EXISTS sc_geometry (scenario_id bigint NOT NULL, id bigint NOT NULL, last_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, PRIMARY KEY (scenario_id, id));
  CREATE TABLE IF NOT EXISTS sc_geometry_history (scenario_id bigint NOT NULL, id bigint NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp, alive bool DEFAULT 'TRUE' NOT NULL, PRIMARY KEY (scenario_id, id, ts_update));
  IF NOT EXISTS
           ( SELECT 1
             FROM information_schema.columns
             WHERE table_name='sc_geometry_history' AND column_name='geom'
           ) THEN
    PERFORM AddGeometryColumn ('sc_geometry_history','geom',4326,'Geometry',3);
  END IF;
  CREATE TABLE IF NOT EXISTS sc_geometry_prop (scenario_id bigint NOT NULL, geometry_id bigint NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name));
  CREATE TABLE IF NOT EXISTS sc_geometry_prop_history (scenario_id bigint NOT NULL, geometry_id bigint NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp DEFAULT NULL, alive bool DEFAULT 'TRUE' NOT NULL, value jsonb NOT NULL, PRIMARY KEY (scenario_id, geometry_id, name, ts_update));
  CREATE TABLE IF NOT EXISTS scenario (id bigserial, alive bool DEFAULT 'TRUE' NOT NULL, name text NOT NULL, lon decimal DEFAULT NULL, lat decimal DEFAULT NULL, alt decimal NOT NULL DEFAULT 0, srid integer NOT NULL, owner bigint, PRIMARY KEY (id));
  CREATE TABLE IF NOT EXISTS scenario_prop (scenario_id bigint NOT NULL, name text NOT NULL, last_update timestamp DEFAULT NULL NOT NULL, PRIMARY KEY (scenario_id, name));
  CREATE TABLE IF NOT EXISTS scenario_prop_history (scenario_id bigint NOT NULL, name text NOT NULL, ts_update timestamp DEFAULT CURRENT_TIMESTAMP NOT NULL, ts_prev_update timestamp DEFAULT NULL, alive bool DEFAULT 'TRUE' NOT NULL, value jsonb NOT NULL, PRIMARY KEY (scenario_id, name, ts_update));
  CREATE INDEX IF NOT EXISTS sc_geometry_scenario_id ON sc_geometry (scenario_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_id ON sc_geometry (id);
  CREATE INDEX IF NOT EXISTS sc_geometry_history_scenario_id ON sc_geometry_history (scenario_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_history_id ON sc_geometry_history (id);
  CREATE INDEX IF NOT EXISTS sc_geometry_history_ts_update ON sc_geometry_history (ts_update);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_scenario_id ON sc_geometry_prop (scenario_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_geometry_id ON sc_geometry_prop (geometry_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_name ON sc_geometry_prop (name);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_history_scenario_id ON sc_geometry_prop_history (scenario_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_history_geometry_id ON sc_geometry_prop_history (geometry_id);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_history_name ON sc_geometry_prop_history (name);
  CREATE INDEX IF NOT EXISTS sc_geometry_prop_history_ts_update ON sc_geometry_prop_history (ts_update);
  CREATE UNIQUE INDEX IF NOT EXISTS scenario_id ON scenario (id);
  CREATE INDEX IF NOT EXISTS scenario_prop_scenario_id ON scenario_prop (scenario_id);
  CREATE INDEX IF NOT EXISTS scenario_prop_name ON scenario_prop (name);
  CREATE INDEX IF NOT EXISTS scenario_prop_history_scenario_id ON scenario_prop_history (scenario_id);
  CREATE INDEX IF NOT EXISTS scenario_prop_history_name ON scenario_prop_history (name);
  CREATE INDEX IF NOT EXISTS scenario_prop_history_ts_update ON scenario_prop_history (ts_update);

  -- add constraints
  BEGIN
    ALTER TABLE sc_geometry_prop ADD CONSTRAINT latest_geom_prop FOREIGN KEY (scenario_id, geometry_id, name, last_update) REFERENCES sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "latest_geom_prop" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry ADD CONSTRAINT latest_geometry FOREIGN KEY (scenario_id, id, last_update) REFERENCES sc_geometry_history (scenario_id, id, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "latest_geometry" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop ADD CONSTRAINT latest_sc_prop FOREIGN KEY (scenario_id, name, last_update) REFERENCES scenario_prop_history (scenario_id, name, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "latest_sc_prop" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_prop_history ADD CONSTRAINT prev_geom_prop_version FOREIGN KEY (scenario_id, geometry_id, name, ts_prev_update) REFERENCES sc_geometry_prop_history (scenario_id, geometry_id, name, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "prev_geom_prop_version" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_history ADD CONSTRAINT prev_geometry_version FOREIGN KEY (scenario_id, id, ts_prev_update) REFERENCES sc_geometry_history (scenario_id, id, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "prev_geometry_version" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop_history ADD CONSTRAINT prev_sc_prop_version FOREIGN KEY (scenario_id, name, ts_prev_update) REFERENCES scenario_prop_history (scenario_id, name, ts_update);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "prev_sc_prop_version" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry_prop ADD CONSTRAINT sc_geometry_properties FOREIGN KEY (scenario_id, geometry_id) REFERENCES sc_geometry (scenario_id, id);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "sc_geometry_properties" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE sc_geometry ADD CONSTRAINT scenario_content FOREIGN KEY (scenario_id) REFERENCES scenario (id);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "scenario_content" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario_prop ADD CONSTRAINT scenario_properies FOREIGN KEY (scenario_id) REFERENCES scenario (id);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "scenario_properies" already exists, skipping.';
  END;
  BEGIN
    ALTER TABLE scenario ADD CONSTRAINT scenario_srid FOREIGN KEY (srid) REFERENCES spatial_ref_sys (srid);
  EXCEPTION
    WHEN duplicate_object THEN RAISE NOTICE 'Constraint "scenario_srid" already exists, skipping.';
  END;
END;
$$ LANGUAGE plpgsql;
