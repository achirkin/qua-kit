CREATE OR REPLACE FUNCTION wrap_result(token bigint, result jsonb)
  RETURNS jsonb AS
$$
BEGIN
  RETURN (
    SELECT jsonb_build_object(
      'result', result,
      'callID', token
    )
  );
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION wrap_progress(token bigint, progress decimal, result jsonb)
  RETURNS jsonb AS
$$
BEGIN
  RETURN (
    SELECT jsonb_build_object(
      'intermediateResult', result,
      'callID', token,
      'progress', progress
    )
  );
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION wrap_progress_many(tokens bigint ARRAY, progress decimal, result jsonb)
  RETURNS setof jsonb AS
$$
BEGIN
  RETURN QUERY
    SELECT jsonb_build_object
      ( 'intermediateResult', result
      , 'callID', t
      , 'progress', progress
      )
    FROM unnest(tokens) t;

END;
$$ LANGUAGE plpgsql;
