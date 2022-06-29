-- S-22
SELECT level_id,
        institutional_cumulative_credits_earned
FROM student_term_level
WHERE level_id = 'GR'
LIMIT 10;
