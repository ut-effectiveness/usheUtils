-- S-23
SELECT institutional_cumulative_gpa,
       level_id
FROM student_term_level
WHERE level_id = 'GR'
LIMIT 10;
