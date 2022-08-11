-- Program File Submission
SELECT DISTINCT(a.major_desc),
                a.cip_code,
                a.ipeds_award_level_code,
                a.degree_id,
                a.is_perkins,
                'N' AS "technical_education",
                MAX(a.required_credits) as required_credits,
                'N' AS "program_participation_agreement"
FROM export.academic_programs a
WHERE required_credits IS NOT NULL
GROUP BY major_desc, cip_code, ipeds_award_level_code, degree_id, is_perkins, technical_education, program_participation_agreement ;
