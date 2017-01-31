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
