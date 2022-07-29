-- Program File Submission with versioning
SELECT DISTINCT(a.major_desc),
                a.cip_code,
                a.ipeds_award_level_code,
                a.degree_id,
                a.is_perkins,
                'N' AS "technical_education",
                a.required_credits,
                '' AS "program_participation_agreement", -- may need make function if fin aid says yes otherwise hard code N
                a.version_desc,
                b.term_id
FROM export.academic_programs_version a
LEFT JOIN export.student_term_level_version b
    ON a.version_id = b.version_id
WHERE required_credits IS NOT NULL
AND b.term_id >= '202220';
