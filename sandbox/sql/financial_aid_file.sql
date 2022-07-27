--USHE Financial Aid Script in edify
SELECT d.academic_year_code as year,
       d.season,
       c.ssn as f_id,
       b.student_id as banner_id,
       b.financial_aid_fund_id,
       b.amount_offered as financial_aid_amount,
       b.financial_aid_year_id
FROM export.student_term_level_version a
    LEFT JOIN export.student_financial_aid_year_fund_term_detail b on b.student_id = a.student_id and b.term_id = a.term_id
    LEFT JOIN export.student c on c.student_id = b.student_id
    LEFT JOIN export.term d on d.term_id = b.term_id
WHERE b.amount_offered > '0'
AND version_desc in ('Census', 'End of Term')
Order by b.student_id;

