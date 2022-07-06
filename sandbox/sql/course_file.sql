/* Course File with version */
      SELECT DISTINCT
             a.term_id,
             a.course_number,
             a.section_number,
             a.subject_code,
             b.instructor_employee_id,
             c.meet_building_id AS meet_building_id_1,
             d.meet_building_id AS meet_building_id_2,
             e.meet_building_id AS meet_building_id_3,
             c.building_number AS building_number_1,
             d.building_number AS building_number_2,
             e.building_number AS building_number_3,
             a.budget_code,
             a.class_size,
             a.college_id,
             a.contact_hours,
             -- c_credit_ind
             c.meet_days AS meet_days_1,
             d.meet_days AS meet_days_2,
             e.meet_days AS meet_days_3,
             a.instruction_method_code,
             a.academic_department_id,
             -- course_destination_site
             c.meet_end_date,
             --c-47 gen ed uses attribute_code, subject_code, course_number
             g.first_name || ' ' || g.last_name AS instructor_name,
             a.section_format_type_code,
             -- class_level,
             -- c_line_item,
             COALESCE(f.course_max_credits, f.course_min_credits) AS course_max_credits,
             f.course_min_credits,
             f.program_type,
             c.meet_room_number,
             d.meet_room_number,
             e.meet_room_number,
             c.room_max_occupancy AS room_max_occupancy_1,
             d.room_max_occupancy AS room_max_occupancy_2,
             e.room_max_occupancy AS room_max_occupancy_3,
             c.room_use_code AS room_use_code_1,
             d.room_use_code AS room_use_code_2,
             e.room_use_code AS room_use_code_3,
             a.campus_id,
             c.meet_start_time,
             d.meet_start_time,
             e.meet_start_time,
             c.meet_end_time,
             d.meet_end_time,
             e.meet_end_time,
             f.course_title,
             a.course_reference_number,
             a.version_desc
        FROM export.student_section_version a
   LEFT JOIN export.section_instructor_assignment_version b
          ON b.section_id = a.section_id
         AND b.is_primary_instructor
         AND b.version_snapshot_id = a.version_snapshot_id
   /* Pivot buildings on rooms based on building_room_rank */
   LEFT JOIN export.section_schedule_version c
          ON c.section_id = a.section_id
         AND c.building_room_rank = '1'
         AND c.version_snapshot_id = a.version_snapshot_id
   LEFT JOIN export.section_schedule_version d
          ON d.section_id = a.section_id
         AND d.building_room_rank = '2'
         AND d.version_snapshot_id = a.version_snapshot_id
   LEFT JOIN export.section_schedule_version e
          ON e.section_id = a.section_id
         AND e.building_room_rank = '3'
         AND e.version_snapshot_id = a.version_snapshot_id
   LEFT JOIN export.course_version f
          ON f.course_id = a.course_id
         AND f.version_snapshot_id = a.version_snapshot_id
   LEFT JOIN export.employee_version g
          ON g.employee_id = b.instructor_employee_id
         AND g.version_snapshot_id = a.version_snapshot_id
   LEFT JOIN export.term h
          ON h.term_id = a.term_id
       WHERE a.is_enrolled = TRUE
         AND a.term_id >= (SELECT term_id FROM export.term WHERE is_previous_term) -- Previous Term and forward
    ORDER BY a.term_id, a.course_number;
