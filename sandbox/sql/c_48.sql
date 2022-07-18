-- C-48
SELECT DISTINCT (a.section_number),
                a.budget_code,
                instruction_method_code,
                campus_id
FROM student_section a
WHERE (budget_code = 'SF' OR budget_code = 'BC');
