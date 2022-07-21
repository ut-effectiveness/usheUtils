-- USHE Missionary File
SELECT a.last_name,
       a.first_name,
       a.middle_name,
       a.birth_date,
       c.term_end_date,
       b.term_id
FROM export.student a
LEFT JOIN export.student_term_level b
ON a.student_id = b.student_id
LEFT JOIN export.term c
ON b.term_id = c.term_id;
