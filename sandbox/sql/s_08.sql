-- S-08
SELECT local_address_zip_code,
       mailing_address_zip_code
FROM student
WHERE is_active
LIMIT 10;
