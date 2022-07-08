## code to prepare `fake_student_df` dataset goes here

sample_size <- 10000

fake_student_df <- data.frame(
  student_id = stringr::str_pad( sample(1:999999, sample_size) , 8, pad = '0'),
  previous_student_id = as.character( sample(1:999999, sample_size) ),
  student_ssn = sample(c("123-45-6789", NA), sample_size, replace = TRUE),
  gender_code = sample(c("M", "F", NA), sample_size, replace = TRUE),
  last_name = sample(c("Doe", "Smith", "Johnson", "Williams"), sample_size, replace = TRUE),
  first_name = sample(c("Jane", "Joe", "Robert", "Mary"), sample_size, replace = TRUE),
  middle_name = sample(c("Elizabeth", "Louise", "Ann", "Lee"), sample_size, replace = TRUE),
  name_suffix = sample(c("Jr.", "Sr.", "I", "II", "III"), sample_size, replace = TRUE),
  previous_last_name = sample(c("Doe", "Smith", "Johnson", "Williams"), sample_size, replace = TRUE),
  previous_first_name = sample(c("Jane", "Joe", "Robert", "Mary"), sample_size, replace = TRUE),
  previous_middle_name = sample(c("Elizabeth", "Louise", "Ann", "Lee"), sample_size, replace = TRUE),
  previous_name_suffix = sample(c("Jr.", "Sr.", "I", "II", "III"), sample_size, replace = TRUE),
  academic_year = as.character( sample(1978:2022, sample_size, replace = TRUE) ),
  season = sample(c("Fall", "Spring", "Summer"), sample_size, replace = TRUE),
  version_id = sample(c("1", "2", "3"), sample_size, replace = TRUE),
  birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size),
  is_hispanic_latino_ethnicity = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_asian = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_black = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_american_indian_alaskan = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_hawaiian_pacific_islander = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_white = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_international = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  is_other_race = sample(c(TRUE, FALSE), sample_size, replace = TRUE),
  local_address_zip_code = sample(c("123456-7890", NA), sample_size, replace = TRUE),
  mailing_address_zip_code = sample(c("987654-3210", NA), sample_size, replace = TRUE),
  us_citizenship_code = sample(c("1", "2", "3", "4", "5", "6", "9", NA), sample_size, replace = TRUE),
  first_admit_county_code = sample(c("015", "01", "039", NA), sample_size, replace = TRUE),
  first_admit_country_iso_code = sample(c("US", "XX", NA), sample_size, replace = TRUE),
  first_admit_state_code = sample(c("UT","CA","AZ", NA), sample_size, replace = TRUE),
  residency_code = sample(c("S", "R", "A", "N", "C", "0", "H", "M", "G", NA), sample_size, replace = TRUE),
  primary_major_cip_code = sample(c("100305", "430403", "511505", "090101", NA), sample_size, replace = TRUE),
  student_type_code = sample(c("3", "S", "2", "R", "5", "N", "C", "0", "P", "H", "1", "F", "T", NA), sample_size, replace = TRUE),
  primary_level_class_id = sample(c("JR", "SR"," FR", "GG", "SO", NA), sample_size, replace = TRUE),
  primary_degree_id = sample(c("AS", "BME", "MAT", "TR", "AB", "BAS", "BFA", "CER1", "APE", "AC", "MA", "CER0", "AAS", "BIS", "", "BSN", "MACC", "BA", "ND", "BM", "AA", "MMFT", "BS"), sample_size, replace = TRUE),
  institutional_cumulative_credits_earned = sample(0:200, sample_size, replace = TRUE),
  institutional_cumulative_gpa = sample(0:400, sample_size, replace = TRUE) / 100,
  level_id = sample(c("UG", "NC", "GR", "CE", "00", ""), sample_size, replace = TRUE)
)

usethis::use_data(fake_student_df, overwrite = TRUE)
