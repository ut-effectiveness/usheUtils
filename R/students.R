
#' Generate Student Validation File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (list, of, required, data, fields).
#'
#' @return A Data Frame, with all of the intermediate values used to create the USHE elements required for upload submission.
#' @export
#'
#' @examples
#' generate_student_validation_file()
#'
generate_student_validation_file <- function(input_df=usheUtils::fake_student_df) {

  original_column_names <- colnames(input_df)

  output_df <- input_df %>%
    s_01(with_intermediates = TRUE) %>%
    s_02(with_intermediates = TRUE) %>%
    s_03(with_intermediates = TRUE) %>%
    s_04(with_intermediates = TRUE) %>%
    s_05(with_intermediates = TRUE) %>%
    s_06(with_intermediates = TRUE) %>%
    s_07(with_intermediates = TRUE) %>%
    s_08(with_intermediates = TRUE) %>%
    s_09(with_intermediates = TRUE) %>%
    s_10(with_intermediates = TRUE) %>%
    s_11(with_intermediates = TRUE) %>%
    s_12(with_intermediates = TRUE) %>%
    s_13(with_intermediates = TRUE) %>%
    s_14(with_intermediates = TRUE) %>%
    s_15(with_intermediates = TRUE) %>%
    s_16(with_intermediates = TRUE) %>%
    s_17(with_intermediates = TRUE) %>%
    s_18(with_intermediates = TRUE) %>%
    s_19(with_intermediates = TRUE) %>%
    s_20(with_intermediates = TRUE) %>%
    s_21(with_intermediates = TRUE) %>%
    s_22(with_intermediates = TRUE) %>%
    s_23(with_intermediates = TRUE) %>%
    dplyr::select( -c("s_01", "s_02", "s_03", "s_04",
                      "s_05", "s_06", "s_07", "s_08",
                      "s_09", "s_10", "s_11", "s_12",
                      "s_13", "s_14", "s_15", "s_16",
                      "s_17", "s_18", "s_19", "s_20",
                      "s_21", "s_22", "s_23") ) %>%
    dplyr::select( -c(original_column_names) )

  return(output_df)
}


#' Generate Student Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (list, of, required, data, fields).
#'
#' @return A Data Frame, with all of the USHE elements required for upload submission.
#' @export
#'
#' @examples
#' generate_student_submission_file()
#'
generate_student_submission_file <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    s_01() %>%
    s_02() %>%
    s_03() %>%
    s_04() %>%
    s_05() %>%
    s_06() %>%
    s_07() %>%
    s_08() %>%
    s_09() %>%
    s_10() %>%
    s_11() %>%
    s_12() %>%
    s_13() %>%
    s_14() %>%
    s_15() %>%
    s_16() %>%
    s_17() %>%
    s_18() %>%
    s_19() %>%
    s_20() %>%
    s_21() %>%
    s_22() %>%
    s_23() %>%
    dplyr::select( c("s_01", "s_02", "s_03", "s_04",
                     "s_05", "s_06", "s_07", "s_08",
                     "s_09", "s_10", "s_11", "s_12",
                     "s_13", "s_14", "s_15", "s_16",
                     "s_17", "s_18", "s_19", "s_20",
                     "s_21", "s_22", "s_23") )

  return(output_df)
}


#' Calculate USHE Element s_xx (Name of Element)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (list, of, required, data, fields).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_xx appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_xx()
#'
s_xx <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( intermediate_field_1 = "1") %>%
    mutate( intermediate_field_2 = "2") %>%
    # Append USHE data element s_xx
    mutate( s_xx = paste0(intermediate_field_1, intermediate_field_2) )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(intermediate_field_1, intermediate_field_2) )
  }

  return(output_df)
}


#' Calculate USHE Element s_01 (Institution)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institution
#' - FIELD NAME: C_INST, S_INST, G_INST, SC_INST, M_INST, F_INST, R_INST, B_INST, & PF_INST
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The identification number used by the U.S. Department of Education’s Office of Postsecondary Education (OPE) to identify schools that have Program Participation Agreements (PPA) so that its students are eligible to participate in Federal Student Financial Assistance programs under Title IV regulations.
#'               This is a 6-digit number followed by a 2-digit suffix used to identify branches,  additional locations, and other entities that are part of the eligible institution.
#'               USHE will use the last 4 digits of the 6-digit number for the field value as additional locations will be specified by C-09 Line Item.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (no data fields required).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_01 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_01()
#'
s_01 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_inst = "3671") %>%
    # Append USHE data element s_01
    mutate( s_01 = s_inst )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_inst) )
  }

  return(output_df)
}

#' Calculate USHE Element s_02 (Year, Term, & Extract)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year, Term & Extract Code
#' - FIELD NAME: C_TERM_ID
#' - FIELD FORMAT: Number, 4 Characters (YYYY format)
#' - Number, 1 Character (T format)
#' - Varchar, 1 character (E format)
#' - DEFINITION: The current academic year, term, and extract in which course is offered.
#'               Format YYYYTE is made up of academic year YYYY, academic term T, and extract E (3rd week or End of Term).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (season, academic_year, version_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_02 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_02()
#'
s_02 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_year = case_when(
      season == "Summer" ~ as.character( (as.numeric(academic_year) + 1) ),
      season == "Fall" ~ as.character( (as.numeric(academic_year) + 1) ),
      season == "Spring" ~ as.character( academic_year ),
      TRUE ~ " ")) %>%
    mutate(s_term = case_when(
      season == "Summer" ~ '1',
      season == "Fall" ~ '2',
      season == "Spring" ~ '3',
      TRUE ~ " ")) %>%
    mutate(s_extract = case_when(
      # 'C' for current
      version_id == '1' ~ 'C',
      # '3' for 3rd term
      version_id == '2' ~ '3',
      # 'E' for End of Term
      version_id == '3' ~ 'E',
      TRUE ~ " ")) %>%
    # Append USHE data element s_02
    mutate(s_02 = paste0(s_year, s_term, s_extract) )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # remove fields used for intermediate calculations
      select( -c(s_year, s_term, s_extract) )
  }

  return(output_df)
}


#' Calculate USHE Element s_03 (Student ID)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institutionally Assigned ID
#' - FIELD NAME: G_BANNER_ID
#' - FIELD FORMAT: Varchar, 9 Characters
#' - DEFINITION: The unique institutionally assigned identification number for each student intended to be used in lieu of using a student’s social security number.
#'               G_Banner_ID must begin with an Alpha character representing the institution.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id, student_ssn).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_03()
#'
s_03 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_id = if_else(is.na(student_ssn),
                           paste0('D', student_id),
                           gsub('-', '', student_ssn) ) ) %>%
    # Append USHE data element s_03
    mutate( s_03 = s_id  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_id) )
  }

  return(output_df)
}


#' Calculate USHE Element s_04 (Student ID Flag)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Student ID Flag
#' - FIELD NAME: S_ID_FLAG
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: Flag indicating if Student ID is the student's actual Social Security Number or an institutionally assigned ID number (to identify students who don't have SSN's, i.e. International  Students).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_id, student_ssn).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_04()
#'
s_04 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_id_flag = if_else(is.na(student_ssn),
                                'I',
                                'S' ) ) %>%
    # Append USHE data element s_04
    mutate( s_04 = s_id_flag  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_id_flag) )
  }

  return(output_df)
}

#' Calculate USHE Element s_05 (Previous ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (previous_student_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_05 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_05()
#'
s_05 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_previous_id = previous_student_id) %>%
    # Append USHE data element s_05
    mutate( s_05 = previous_student_id )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(previous_student_id) )
  }

  return(output_df)
}

#' Calculate USHE Element s_06 (Student Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name, name_suffix).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_06()
#'
s_06 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_last = last_name,
            s_first = first_name,
            s_middle = middle_name,
            s_suffix = name_suffix ) %>%
    # Append USHE data element s_06
    mutate( s_06 = paste(s_last, s_first, s_middle, s_suffix, sep=' | ') )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_last, s_middle, s_middle, s_suffix) )
  }

  return(output_df)
}


#' Calculate USHE Element s_07 (Student Previous Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (previous_last_name, previous_first_name, previous_middle_name, previous_name_suffix).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_07 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_07()
#'
s_07 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_prev_last = previous_last_name,
            s_prev_first = previous_first_name,
            s_prev_middle = previous_middle_name,
            s_prev_suffix = previous_name_suffix ) %>%
    # Append USHE data element s_07
    mutate( s_07 = paste(s_prev_last, s_prev_first, s_prev_middle, s_prev_suffix, sep=' | ') )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_prev_last, s_prev_middle, s_prev_middle, s_prev_suffix) )
  }

  return(output_df)
}


#' Calculate USHE Element s_08 (Zip Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Current Zip Code
#' - FIELD NAME: S_CURR_ZIP
#' - FIELD FORMAT: Varchar, 10 Characters
#' - DEFINITION: The postal code of the student's current local address while attending classes.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (local_address_zip_code, mailing_address_zip_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_08 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_08()
#'
s_08 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_curr_zip = coalesce( local_address_zip_code, mailing_address_zip_code ) ) %>%
    # Append USHE data element s_08
    mutate( s_08 = s_curr_zip )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_curr_zip) )
  }

  return(output_df)
}


#' Calculate USHE Element s_09 (Citizenship Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Citizenship Status
#' - FIELD NAME: S_CITZ_CODE
#' - FIELD FORMAT: Varchar, 1 Character
#' - DEFINITION: Indicates student's current citizenship status.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (us_citizenship_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_09 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_09()
#'
s_09 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_citz_code = case_when( us_citizenship_code == "6" ~ "9",
                                    TRUE ~ us_citizenship_code) ) %>%
    # Append USHE data element s_09
    mutate( s_09 = s_citz_code )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_citz_code) )
  }

  return(output_df)
}

#' Calculate USHE Element s_10 (Utah County Code)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Utah County Code
#' - FIELD NAME: S_COUNTY_ORIGIN
#' - FIELD FORMAT: Varchar, 5 Characters,
#' - DEFINITION: The Utah county code indicating the student’s county of origin as described at the time of first application to the institution for enrollment and if the S_STATE_ORIGIN is UT.
#'               Enter UT030 if  county is Unknown.
#'               Enter UT097 if student is Out of State, Out of US.
#'               Enter UT099 if student is Out  of State, In the US.
#'               This element should be logically consistent with S-11: S_STATE_ORIGIN and S 27: S_COUNTRY_ORIGIN.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_county_code, first_admit_state_code, first_admit_country_iso_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_10 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_10()
#'
s_10 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_county_origin = case_when(
      ( first_admit_state_code == "UT" & !is.na(first_admit_county_code) ) ~ paste0("UT", first_admit_county_code),
      first_admit_country_iso_code == "US" ~ "UT099",
      first_admit_country_iso_code != "US" ~ "UT097",
      TRUE ~ "UT030")) %>%
    # Append USHE data element s_10
    mutate( s_10 = s_county_origin )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_county_origin) )
  }

  return(output_df)
}

#' Calculate USHE Element s_11 (State Origin Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_state_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_11 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_11()
#'
s_11 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_state_origin = first_admit_state_code ) %>%
    # Append USHE data element s_11
    mutate( s_11 = s_state_origin )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_state_origin) )
  }

  return(output_df)
}


#' Calculate USHE Element s_12 (Birth Date)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Date of Birth
#' - FIELD NAME: S_BIRTH_DT
#' - FIELD FORMAT: Varchar, 8 Characters (YYYYMMDD),
#' - DEFINITION: The calendar student date of birth, as designated by student.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (birth_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_12 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_12()
#'
s_12 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_birth_dt = gsub("-", "", birth_date) ) %>%
    # Append USHE data element s_12
    mutate( s_12 = s_birth_dt )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_birth_dt) )
  }

  return(output_df)
}


#' Calculate USHE Element s_13 (Gender)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (gender_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_13 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_13()
#'
s_13 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_gender = gender_code ) %>%
    # Append USHE data element s_11
    mutate( s_13 = s_gender )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_gender) )
  }

  return(output_df)
}


#' Calculate USHE Element s_14 (Ethnic Origin)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Ethnic Origin
#' - FIELD NAME: S_ETHNIC
#' - FIELD FORMAT: Varchar, 8 Characters,
#' - DEFINITION: The racial and ethnic categories used to classify students.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_hispanic_latino_ethnicity, is_asian, is_black, is_american_indian_alaskan, is_hawaiian_pacific_islander, is_white, is_international, is_other_race).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_14 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_14()
#'
s_14 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_ethnic_h = if_else(is_hispanic_latino_ethnicity, "H", " "),
           s_ethnic_a = if_else(is_asian, "A", " "),
           s_ethnic_b = if_else(is_black, "B", " "),
           s_ethnic_i = if_else(is_american_indian_alaskan, "I", " "),
           s_ethnic_p = if_else(is_hawaiian_pacific_islander, "P", " "),
           s_ethnic_w = if_else(is_white, "W", " "),
           s_ethnic_n = if_else(is_international, "N", " "),
           s_ethnic_u = if_else(is_other_race, "U", " ") ) %>%
    # Append USHE data element s_14
    mutate(s_14 = paste0(s_ethnic_h, s_ethnic_a, s_ethnic_b, s_ethnic_i, s_ethnic_p, s_ethnic_w, s_ethnic_n, s_ethnic_u) )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # remove fields used for intermediate calculations
      select( -c(s_ethnic_h, s_ethnic_a, s_ethnic_b, s_ethnic_i, s_ethnic_p, s_ethnic_w, s_ethnic_n, s_ethnic_u) )
  }

  return(output_df)
}

#' Calculate USHE Element s_15 (Residency)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Regent Residency Status
#' - FIELD NAME: S_REGENT_RES
#' - FIELD FORMAT: Varchar, 1 Character,
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (residency_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_15 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_15()
#'
s_15 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_regent_res = case_when( residency_code == "R" ~ 'R',
                                      residency_code == "C" ~ 'R',
                                      residency_code == "N" ~ 'N',
                                      residency_code == "S" ~ ' ',
                                      residency_code == "H" ~ 'N',
                                      residency_code == "0" ~ ' ',
                                      residency_code == "A" ~ 'A',
                                      residency_code == "M" ~ 'M',
                                      residency_code == "G" ~ 'G') ) %>%
    # Append USHE data element s_xx
    mutate( s_15 = s_regent_res )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_regent_res) )
  }

  return(output_df)
}


#' Calculate USHE Element s_16 (CIP Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_cip_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_16 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_16()
#'
s_16 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_curr_cip = primary_major_cip_code) %>%
    # Append USHE data element s_16
    mutate( s_16 = s_curr_cip )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_curr_cip) )
  }

  return(output_df)
}


#' Calculate USHE Element s_17 (Registration Status)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Registration Status
#' - FIELD NAME: S_REG_STATUS
#' - FIELD FORMAT: Varchar, 2 Characters,
#' - DEFINITION: Student's entry status at the beginning of the term.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_type_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_17 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_17()
#'
s_17 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_reg_status = case_when( student_type_code == "0" ~ '?', # Undeclared
                                     student_type_code == "1" ~ 'NG', # New Graduate
                                     student_type_code == "2" ~ 'TG', # Transfer Graduate
                                     student_type_code == "3" ~ 'RG', # Readmit Graduate
                                     student_type_code == "5" ~ 'CG', # Continuing Graduate
                                     student_type_code == "C" ~ 'CS', # Continuing Registration
                                     student_type_code == "F" ~ 'FF', # Freshman
                                     student_type_code == "H" ~ 'HS', # High School
                                     student_type_code == "N" ~ 'FH', # New Freshman from HS
                                     student_type_code == "P" ~ 'CE', # Personal Interest, Non-Degree
                                     student_type_code == "R" ~ 'RS', # Readmit
                                     student_type_code == "S" ~ 'NC?', # Special
                                     student_type_code == "T" ~ 'TU', # Transfer
                                     ) ) %>%
    # Append USHE data element s_17
    mutate( s_17 = s_reg_status )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_reg_status) )
  }

  return(output_df)
}


#' Calculate USHE Element s_18 (Class Level)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_level_class_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_18 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_18()
#'
s_18 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_level = primary_level_class_id) %>%
    # Append USHE data element s_18
    mutate( s_18 = s_level )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_level) )
  }

  return(output_df)
}


#' Calculate USHE Element s_19 (Degree Intent)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Degree Intent
#' - FIELD NAME: S_DEG_INTENT
#' - FIELD FORMAT: Varchar, 2 Character
#' - DEFINITION: The length/nature of the degree that the student is working toward.
#'               S_DEG_INTENT with S_CURR_CIP represents the student’s major and ties to S_MAJOR
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_degree_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_19 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_19()
#'
s_19 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_deg_intent = case_when( primary_degree_id == "AS" ~ '3',
                                     primary_degree_id == "BME" ~ '5',
                                     primary_degree_id == "MAT" ~ '7',
                                     primary_degree_id == "TR" ~ '0', #?
                                     primary_degree_id == "AB" ~ '3',
                                     primary_degree_id == "MAT" ~ '7',
                                     primary_degree_id == "BAS" ~ '5',
                                     primary_degree_id == "BFA" ~ '5',
                                     primary_degree_id == "CER1" ~ '?', #?
                                     primary_degree_id == "APE" ~ '3',
                                     primary_degree_id == "AC" ~ '3',
                                     primary_degree_id == "MA" ~ '7',
                                     primary_degree_id == "CER0" ~ '?', #?
                                     primary_degree_id == "AAS" ~ '3',
                                     primary_degree_id == "BIS" ~ '5',
                                     primary_degree_id == "" ~ '0', #?
                                     primary_degree_id == "BSN" ~ '5',
                                     primary_degree_id == "MACC" ~ '7',
                                     primary_degree_id == "BA" ~ '5',
                                     primary_degree_id == "ND" ~ '0', #?
                                     primary_degree_id == "BM" ~ '5',
                                     primary_degree_id == "AA" ~ '3',
                                     primary_degree_id == "MMFT" ~ '7',
                                     primary_degree_id == "BS" ~ '5') ) %>%
    # Append USHE data element s_19
    mutate( s_19 = s_deg_intent )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_deg_intent) )
  }

  return(output_df)
}

#' Calculate USHE Element s_20 (Total Cumulative Undergrad Hrs)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Undergraduate Hours
#' - FIELD NAME: S_CUM_HRS_UGRAD
#' - FIELD FORMAT: Numeric(5,1)
#' - DEFINITION: Total number of credit hours the student has earned as an undergraduate student at this institution.
#'               Hours should be semester hours. Value should be zero for graduate students (see element S 18).
#'               (Does not include transfer hours.)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_credits_earned).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_20 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_20()
#'
s_20 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_hrs_ugrad = if_else(level_id == "GR",
                                     0,
                                     round(institutional_cumulative_credits_earned, 1))) %>%
    # Append USHE data element s_20
    mutate( s_20 = s_cum_hrs_ugrad )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_cum_hrs_ugrad) )
  }

  return(output_df)
}


#' Calculate USHE Element s_21 (Cumulative Inst Undergrad GPA)
#'
#' @details
#'
#' **USHE Documentation**
#' * ELEMENT NAME: Cumulative Institutional Undergraduate GPA
#' * FIELD NAME: S_CUM_GPA_UGRAD
#' * FIELD FORMAT: Numeric (4,3)
#' * DEFINITION: Student's Cumulative GPA as of the present academic term as an undergraduate student.
#'               This should tie to Cumulative Institutional Hours field.
#'               No transfer hours are to be included.
#'               All credit hours should represent average course grade on a 4.0 scale.
#'               This GPA ties to earned credits in  Cumulative Institutional Undergraduate Hours.
#'               Value should be zero for graduate students (see element  S-18).
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_gpa).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_21 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_21()
#'
s_21 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_gpa_ugrad = if_else(level_id == "GR",
                                    0,
                                    round(institutional_cumulative_gpa, 2))) %>%
    # Append USHE data element s_20
    mutate( s_21 = s_cum_gpa_ugrad )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_cum_gpa_ugrad) )
  }

  return(output_df)
}


#' Calculate USHE Element s_22 (Total Cumulative Inst Grad Hrs)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Graduate Hours
#' - FIELD NAME: S_CUM_HRS_GRAD
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of credit hours the student has earned as a graduate student at this institution.
#'               Hours should be semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_credits_earned).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_22 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_22()
#'
s_22 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_hrs_grad = if_else(level_id == "GR",
                                    round(institutional_cumulative_credits_earned, 1),
                                    0) ) %>%
    # Append USHE data element s_22
    mutate( s_22 = s_cum_hrs_grad )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_cum_hrs_grad) )
  }

  return(output_df)
}


#' Calculate USHE Element s_23 (Cumulative Inst Grad GPA)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Institutional Graduate GPA
#' - FIELD NAME: S_CUM_GPA_GRAD
#' - FIELD FORMAT: Numeric (4,3)
#' - DEFINITION: Student's Cumulative GPA as of the present academic term as a graduate student.
#'               All credit hours should represent average course grade on a 4.0 scale.
#'               This GPA ties to earned credits in Cumulative Institutional Graduate Hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (level_id, institutional_cumulative_gpa).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_23 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_23()
#'
s_23 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_cum_gpa_grad = if_else(level_id == "GR",
                                    round(institutional_cumulative_gpa, 2),
                                    0) ) %>%
    # Append USHE data element s_20
    mutate( s_23 = s_cum_gpa_grad )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_cum_gpa_grad) )
  }

  return(output_df)
}
