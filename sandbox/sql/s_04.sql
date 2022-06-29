-- S-04
SELECT student_id,
       ssn
FROM student
WHERE student_id = '00191662' OR student_id = '00005856'
LIMIT 10;
