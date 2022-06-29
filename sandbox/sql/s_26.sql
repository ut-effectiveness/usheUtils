--S-26
SELECT s.birth_date,
       v.term_id,
       v.version_id,
       v.version_date
FROM student s
LEFT JOIN student_term_level_version v
ON s.student_id = v.student_id
WHERE version_id = '3'
AND term_id = '202220'
LIMIT 10;
