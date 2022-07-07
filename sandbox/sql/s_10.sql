-- S-10
SELECT DISTINCT (first_admit_county_code),
    first_admit_state_code,
    first_admit_country_iso_code
FROM student
LIMIT 10;
