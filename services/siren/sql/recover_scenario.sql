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
