## code to prepare `fake_mission_df` dataset goes here

sample_size <- 3000

fake_mission_df <- data.frame(
  student_id = stringr::str_pad( sample(1:999999, sample_size) , 8, pad = '0'),
  last_name = sample(c("Doe", "Smith", "Johnson", "Williams", NA), sample_size, replace = TRUE),
  first_name = sample(c("Jane", "Joe", "Robert", "Mary", NA), sample_size, replace = TRUE),
  middle_name = sample(c("Elizabeth", "Louise", "Ann", "Lee", NA), sample_size, replace = TRUE),
  start_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE),
  birth_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE),
  end_date = sample(seq(as.Date('1978/01/01'), as.Date('2022/01/01'), by="day"), sample_size, replace = TRUE) )

usethis::use_data(fake_mission_df, overwrite = TRUE)
