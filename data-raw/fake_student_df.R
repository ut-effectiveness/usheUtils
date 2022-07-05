## code to prepare `fake_student_df` dataset goes here

sample_size <- 10000

fake_student_df <- data.frame(
  student_id = stringr::str_pad(sample(1:999999, sample_size), 8, pad = '0'),
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
