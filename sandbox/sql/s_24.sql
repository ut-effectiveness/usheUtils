--S-24
SELECT DISTINCT (student_id),
       COALESCE(a.transfer_cumulative_credits_earned, 0) AS transfer_cumulative_credits_earned,
        COALESCE(a.total_cumulative_ap_credits_earned, 0) AS total_cumulative_ap_credits_earned,
        COALESCE(a.total_cumulative_clep_credits_earned, 0) AS total_cumulative_clep_credits_earned

FROM export.student_term_level_version a
LIMIT 10;
