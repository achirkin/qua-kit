SELECT criterion.id as id
FROM criterion
INNER JOIN problem_criterion
      ON     criterion.id = problem_criterion.criterion_id
         AND problem_criterion.problem_id = ?
LEFT OUTER JOIN
    (SELECT vote.criterion_id as cid,COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0) as m, COALESCE(count(*),0) as n
       FROM vote GROUP BY vote.criterion_id
     UNION ALL
     SELECT  review.criterion_id as cid,0 as m, COALESCE(count(*),0) as n FROM review GROUP BY  review.criterion_id) r
    ON criterion.id = r.cid
GROUP BY criterion.id
ORDER BY SUM(r.m) ASC, SUM(r.n) ASC
LIMIT 1;
