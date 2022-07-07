## code to prepare `fake_student_df` dataset goes here

sample_size <- 10000

fake_student_df <- data.frame(
  student_id = stringr::str_pad( sample(1:999999, sample_size) , 8, pad = '0'),
  previous_student_id = as.character( sample(1:999999, sample_size) ),
  student_ssn = sample(c("123-45-6789", ""), sample_size, replace = TRUE),
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
  is_other_race = sample(c(TRUE, FALSE), sample_size, replace = TRUE)
)

usethis::use_data(fake_student_df, overwrite = TRUE)
