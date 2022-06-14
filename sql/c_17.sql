-- USHE Element C-17
SELECT course_reference_number,
       is_meets_monday,
       is_meets_tuesday,
       is_meets_wednesday,
       is_meets_thursday,
       is_meets_friday,
       is_meets_saturday,
       is_meets_sunday
FROM section_schedule
limit 10;
