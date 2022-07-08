--USHE Financial Aid Script in edify
select DISTINCT '3671' as inst_id,
       d.academic_year_code as year,
       CASE
           WHEN d.season = 'Summer' THEN d.academic_year_code || '1'
           WHEN d.season = 'Fall' THEN d.academic_year_code || '2'
           WHEN d.season = 'Spring' THEN d.academic_year_code || '3'
           END as term,
       c.ssn as f_id,
       b.student_id as banner_id,
CASE WHEN financial_aid_fund_id = 'UADLRN' THEN 'ALEARN'
     WHEN financial_aid_fund_id in ('89331','89330','89698','89699','86181') THEN 'BIA'
     WHEN financial_aid_fund_id = 'UCWSO' THEN 'DWS'
     WHEN financial_aid_fund_id = '83002' THEN 'EDDIS'
     WHEN financial_aid_fund_id = 'UHESSS' THEN 'HESSP'
     WHEN financial_aid_fund_id in ('89800','81002','81041','84004','86084','86017','86028','86086','86142','86173','86099','84005','86006','84011','86111','86118','85103','84009','86188','86091','87320','86112','86068','86050','86058','86003','86076','85055','86135','86114','86150','86153','86140','85026') THEN 'IANB'
     WHEN financial_aid_fund_id = '89602' THEN 'NEWCEN'
     WHEN financial_aid_fund_id = '89600' THEN 'OAID'
     WHEN financial_aid_fund_id = '89551' THEN 'OFAID'
     WHEN financial_aid_fund_id = 'FPERK' THEN 'OFLOAN'
     WHEN financial_aid_fund_id in ('ALTER','ALTERP') THEN 'OLOAN'
     WHEN financial_aid_fund_id in ('89601','83007','89604','81058','80108','83009') THEN 'OSAID'
     WHEN financial_aid_fund_id = 'FPELL' THEN 'PELL'
     WHEN financial_aid_fund_id in ('DPLUS','GPLUS') THEN 'PLUS'
     WHEN financial_aid_fund_id = 'TBPROM' THEN 'PROM'
     WHEN financial_aid_fund_id = '81080' THEN 'PSOCAR'
     WHEN financial_aid_fund_id = '89603' THEN 'REG'
     WHEN financial_aid_fund_id in ('FSEOG','FSEOGS') THEN 'SEOG'
     WHEN financial_aid_fund_id = 'DIRECT' THEN 'SUB'
     WHEN financial_aid_fund_id = 'TDSLFP' THEN 'TALENT'
     WHEN financial_aid_fund_id = '81083' THEN 'TEACHED'
     WHEN financial_aid_fund_id = '89607' THEN 'THBELL'
     WHEN financial_aid_fund_id = 'DUSUB' THEN 'UNSUB'
     WHEN financial_aid_fund_id = '83019' THEN 'VTGAP'
     WHEN financial_aid_fund_id in ('FCWS','FCWSA') THEN 'WORK'
     ELSE 'IANNB'
     END "financial_aid_type",
     b.amount_offered as financial_aid_amount
from export.student_term_level_version a
    left join export.student_financial_aid_year_fund_term_detail b on b.student_id = a.student_id and b.term_id = a.term_id
    left join export.student c on c.student_id = b.student_id
    left join export.term d on d.term_id = b.term_id
where b.financial_aid_year_id = :aid_year  --('1819' - is the 2019 academic year)
  and b.amount_offered > '0'
  and version_desc in ('Census', 'End of Term')
Order by b.student_id, term;
