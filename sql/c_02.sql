-- Just some sql to see if it will work with my function
-- USHE C-02
SELECT DISTINCT (s.course_id),
       t.academic_year_code,
       t.season,
       s.version_id
FROM export.student_section_version s
LEFT JOIN term t
    ON t.term_id = s.term_id
WHERE s.term_id = '202140'
LIMIT 10;
