-- test SQL for USHE C-10
SELECT DISTINCT (course_reference_number),
       campus_id
FROM student_section_version
ORDER BY course_reference_number
LIMIT 10;
