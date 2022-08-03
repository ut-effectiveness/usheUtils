-- Program File Submission
SELECT DISTINCT(a.major_desc),
                a.cip_code,
                a.ipeds_award_level_code,
                a.degree_id,
                a.is_perkins,
                'N' AS "technical_education",
                a.required_credits,
                'N' AS "program_participation_agreement"
FROM export.academic_programs a
WHERE required_credits IS NOT NULL;
