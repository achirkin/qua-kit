-- first query gets all submissions ordered by how many times they were voted
WITH sc AS (
SELECT scenario.*, t.nn, t.mm
FROM scenario
INNER JOIN ( SELECT scenario.author_id as author_id,
                    scenario.task_id as task_id,
                    MAX(scenario.last_update) as last_update,
                    SUM (COALESCE(v.m, 0)) as mm,
                    SUM (COALESCE(v.n, 0))  as nn
             FROM scenario
             LEFT OUTER JOIN
                 ( SELECT vote.better_id as sid,
                          COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0) as m,
                          count(*) as n FROM vote GROUP BY  vote.better_id
                   UNION ALL
                   SELECT vote.worse_id as sid,
                          COALESCE(sum(CASE WHEN vote.voter_id = ? THEN 1 ELSE 0 END),0)  as m,
                          count(*) as n FROM vote GROUP BY  vote.worse_id
                 ) v
                 ON scenario.id = v.sid
             WHERE scenario.author_id != ? AND scenario.task_id = ?
             GROUP BY scenario.author_id, scenario.task_id
             ORDER BY mm ASC, nn ASC
           ) t
        ON    t.author_id = scenario.author_id
          AND t.task_id = scenario.task_id
          AND t.last_update = scenario.last_update
)
-- get all pairs of designs
SELECT s1.id, s2.id
FROM sc s1 CROSS JOIN sc s2
-- select only those pairs, wich never occured for a user
LEFT OUTER JOIN (SELECT vote.better_id, vote.worse_id FROM vote WHERE vote.voter_id = ? AND vote.criterion_id = ?) vv
        ON (vv.better_id = s1.id AND vv.worse_id = s2.id)
        OR (vv.better_id = s2.id AND vv.worse_id = s1.id)
-- remove duplicates and submissions from different exercises
WHERE vv.better_id IS NULL AND s1.id < s2.id AND s1.task_id = s2.task_id
-- order by popularity, but randomize a little
ORDER BY round(random()*4) ASC, (s1.mm + s2.mm) ASC, (s1.nn + s2.nn) ASC
LIMIT 1;
