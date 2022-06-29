--USHE Element S-14
SELECT s.student_id,
       s.is_hispanic_latino_ethnicity,
       s.is_asian,
       s.is_black,
       s.is_american_indian_alaskan,
       s.is_hawaiian_pacific_islander,
       s.is_white,
       s.is_international,
       s.is_other_race
FROM export.student s
limit 10;
