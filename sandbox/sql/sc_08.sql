--SC-08
SELECT DISTINCT(student_id),
       earned_credits,
       version_desc
FROM export.student_section_version
WHERE term_id = '202140'
--AND earned_credits > '0'
ORDER BY student_id
LIMIT 10;
