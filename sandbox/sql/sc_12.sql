-- SC-12
   SELECT DISTINCT(a.budget_code),
          d.season,
          b.high_school_code,
          c.is_concurrent_course,
          b.latest_student_type_code
     FROM export.student_section_version a
LEFT JOIN export.student_version b ON b.student_id = a.student_id
      AND b.version_snapshot_id = a.version_snapshot_id
LEFT JOIN export.course_version c ON c.course_id = a.course_id
      AND c.version_snapshot_id = a.version_snapshot_id
LEFT JOIN export.term d ON d.term_id = a.term_id
    WHERE a.is_enrolled = TRUE
      AND a.term_id >= (SELECT term_id FROM export.term WHERE is_previous_term)
AND b.latest_student_type_code = 'H'
   ORDER BY b.high_school_code
LIMIT 10;


