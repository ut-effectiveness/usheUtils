--S-17
SELECT c.term_start_date,
       b.high_school_graduation_date,
       a.student_type_code,
       a.term_id
FROM student_term_level a
LEFT JOIN student b
ON a.student_id = b.student_id
LEFT JOIN term c
ON a.term_id = c.term_id
WHERE a.term_id = '202220'
AND student_type_code = 'F'
LIMIT 10;
