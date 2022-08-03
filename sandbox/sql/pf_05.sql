--test sql for pf-05
SELECT degree_id
FROM export.academic_programs
WHERE degree_id = 'MMFT'
LIMIT 10;
