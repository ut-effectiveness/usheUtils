-- S-08
SELECT local_address_zip_code,
       mailing_address_zip_code,
       local_address_country,
       mailing_address_country_code,
       first_admit_country_iso_code

FROM export.student_version
WHERE is_active
LIMIT 100;
