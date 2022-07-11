   /* Student Course File with versioning */
   SELECT a.term_id,
          c.season,
          a.student_id,
          a.sis_system_id,
          b.ssn,
          a.subject_code,
          a.course_number,
          a.section_number,
          a.attempted_credits,
          a.earned_credits,
          a.contact_hours,
          a.part_term_weeks,
          a.final_grade,
          b.high_school_code,
          -- membership hours
          b.latest_student_type_code,
          a.budget_code,
          a.course_reference_number,
          a.course_level_id
     FROM export.student_section_version a
LEFT JOIN export.student_version b ON b.student_id = a.student_id
      AND b.version_snapshot_id = a.version_snapshot_id
LEFT JOIN export.term c ON c.term_id = a.term_id
    WHERE a.is_enrolled = TRUE
      AND a.term_id >= (SELECT term_id FROM export.term WHERE is_previous_term)
 ORDER BY a.student_id,
          a.course_reference_number;
