
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
    s_24(with_intermediates = TRUE) %>%
    s_25(with_intermediates = TRUE) %>%
    s_26(with_intermediates = TRUE) %>%
    s_27(with_intermediates = TRUE) %>%
    s_28(with_intermediates = TRUE) %>%
    s_29(with_intermediates = TRUE) %>%
    s_30(with_intermediates = TRUE) %>%
    s_31(with_intermediates = TRUE) %>%
    s_32(with_intermediates = TRUE) %>%
    s_33(with_intermediates = TRUE) %>%
    s_34(with_intermediates = TRUE) %>%
    s_35(with_intermediates = TRUE) %>%
    s_36(with_intermediates = TRUE) %>%
    s_37(with_intermediates = TRUE) %>%
    s_38(with_intermediates = TRUE) %>%
    s_39(with_intermediates = TRUE) %>%
    s_40(with_intermediates = TRUE) %>%
    s_41(with_intermediates = TRUE) %>%
    s_42(with_intermediates = TRUE) %>%
    s_43(with_intermediates = TRUE) %>%
    s_44(with_intermediates = TRUE) %>%
    s_45(with_intermediates = TRUE) %>%
    s_46(with_intermediates = TRUE) %>%
    s_47(with_intermediates = TRUE) %>%
    s_48(with_intermediates = TRUE) %>%
    s_49(with_intermediates = TRUE) %>%
    dplyr::select( -c("s_01", "s_02", "s_03", "s_04",
                      "s_05", "s_06", "s_07", "s_08",
                      "s_09", "s_10", "s_11", "s_12",
                      "s_13", "s_14", "s_15", "s_16",
                      "s_17", "s_18", "s_19", "s_20",
                      "s_21", "s_22", "s_23", "s_24",
                      "s_25", "s_26", "s_27", "s_28",
                      "s_29", "s_30", "s_31", "s_32",
                      "s_33", "s_34", "s_35", "s_36",
                      "s_37", "s_38", "s_39", "s_40",
                      "s_41", "s_42", "s_43", "s_44",
                      "s_45", "s_46", "s_47", "s_48", "s_49") ) %>%
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
    s_24() %>%
    s_25() %>%
    s_26() %>%
    s_27() %>%
    s_28() %>%
    s_29() %>%
    s_30() %>%
    s_31() %>%
    s_32() %>%
    s_33() %>%
    s_34() %>%
    s_35() %>%
    s_36() %>%
    s_37() %>%
    s_38() %>%
    s_39() %>%
    s_40() %>%
    s_41() %>%
    s_42() %>%
    s_43() %>%
    s_44() %>%
    s_45() %>%
    s_46() %>%
    s_47() %>%
    s_48() %>%
    s_49() %>%
    dplyr::select( c("s_01", "s_02", "s_03", "s_04",
                     "s_05", "s_06", "s_07", "s_08",
                     "s_09", "s_10", "s_11", "s_12",
                     "s_13", "s_14", "s_15", "s_16",
                     "s_17", "s_18", "s_19", "s_20",
                     "s_21", "s_22", "s_23", "s_24",
                     "s_25", "s_26", "s_27", "s_28",
                     "s_29", "s_30", "s_31", "s_32",
                     "s_33", "s_34", "s_35", "s_36",
                     "s_37", "s_38", "s_39", "s_40",
                     "s_41", "s_42", "s_43", "s_44",
                     "s_45", "s_46", "s_47", "s_48", "s_49") )

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


#' Calculate USHE Element s_24 (Total Cum U-grad Transfer Hrs Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Cumulative Undergraduate Transfer Accepted
#' - FIELD NAME: S_TRANS_TOTAL
#' - FIELD FORMAT: Numeric (5,1)
#' - DEFINITION: Total number of credit hours accepted to date at your institution (e.g. transfer credit from another institution).
#'               Applies to undergraduate and graduate hours.
#'               This includes credit from other institutions, Challenge, Military, and other test credit except CLEP and AP (see S-32 and S-33).
#'               This does not include credits earned at your institution (see S-20).
#'               Hours should all be converted to semester hours.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (transfer_cumulative_credits_earned, total_cumulative_clep_credits_earned, total_cumulative_ap_credits_earned).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_24 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_24()
#'
s_24 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_trans_total = max( ( as.double(transfer_cumulative_credits_earned) - (as.double(total_cumulative_clep_credits_earned) + as.double(total_cumulative_ap_credits_earned)) ),
                                 0 ) ) %>%
    # Append USHE data element s_24
    mutate(s_24 = s_trans_total)


  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_trans_total) )
  }
  return(output_df)
}


#' Calculate USHE Element s_25 (Part-time/Full-time)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (full_time_part_time_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_25 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_25()
#'
s_25 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_pt_ft = full_time_part_time_code) %>%
    # Append USHE data element s_25
    mutate( s_25 = s_pt_ft )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_pt_ft) )
  }

  return(output_df)
}


#' Calculate USHE Element s_26 (Age)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Age at 3rd Week
#' - FIELD NAME: S_AGE
#' - FIELD FORMAT: Numeric (3,0)
#' - DEFINITION: The age of the student at third week extract date.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (version_date, birth_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_26 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_26()
#'
s_26 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_age = floor( as.numeric( (as.Date(version_date) - as.Date(birth_date)) / 365.24 ) ) ) %>%
    # Append USHE data element s_26
    mutate( s_26 = s_age )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_age) )
  }

  return(output_df)
}


#' Calculate USHE Element s_27 (Country Origin Codes)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (first_admit_country_iso_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_xx appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_27()
#'
s_27 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_country_origin = first_admit_country_iso_code) %>%
    # Append USHE data element s_27
    mutate( s_27 = s_country_origin )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_country_origin) )
  }

  return(output_df)
}

#' Calculate USHE Element s_28 (High School Codes)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (latest_high_school_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_28 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_28()
#'
s_28 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_high_school = latest_high_school_code ) %>%
    # Append USHE data element s_28
    mutate( s_28 = s_high_school )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_high_school) )
  }

  return(output_df)
}


#' Calculate USHE Element s_29 (Non Resident Tuition Waiver)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (house_bill_75_waiver).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_29 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_29()
#'
s_29 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    # TODO: This needs to be implemented.
    mutate( s_hb75_waiver = NA ) %>%
    # Append USHE data element s_29
    mutate( s_29 = s_hb75_waiver )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_hb75_waiver) )
  }

  return(output_df)
}


#' Calculate USHE Element s_30 (CIP Code for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_cip_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_30 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_30()
#'
s_30 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_curr_cip2 = secondary_major_cip_code ) %>%
    # Append USHE data element s_30
    mutate( s_30 = s_curr_cip2 )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_curr_cip2) )
  }

  return(output_df)
}


#' Calculate USHE Element s_31 (Cumulative Membership Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (?).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_31 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_31()
#'
s_31 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    # TODO: This needs to be implemented.
    mutate( s_cum_membership = NA ) %>%
    # Append USHE data element s_31
    mutate( s_31 = s_cum_membership )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_cum_membership) )
  }

  return(output_df)
}


#' Calculate USHE Element s_32 (Total Cum U-grad CLEP Cr Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_cumulative_clep_credits_earned).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_32 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_32()
#'
s_32 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_total_clep = total_cumulative_clep_credits_earned) %>%
    # Append USHE data element s_32
    mutate( s_32 = s_total_clep )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_total_clep) )
  }

  return(output_df)
}


#' Calculate USHE Element s_33 (Total Cum U-grad AP Cr Accepted)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (total_cumulative_ap_credits_earned).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_33 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_33()
#'
s_33 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_total_ap = total_cumulative_ap_credits_earned) %>%
    # Append USHE data element s_33
    mutate( s_33 = s_total_ap )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_total_ap) )
  }

  return(output_df)
}


#' Calculate USHE Element s_34 (SSID aka USOE Unique ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_ssid).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_34 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_34()
#'
s_34 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_ssid = student_ssid) %>%
    # Append USHE data element s_34
    mutate( s_34 = s_ssid )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_ssid) )
  }

  return(output_df)
}

#' Calculate USHE Element s_35 (Institution Assigned ID)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (student_ssid).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_35 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_35()
#'
s_35 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_banner_id = student_id) %>%
    # Append USHE data element s_35
    mutate( s_35 = s_banner_id )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_banner_id) )
  }

  return(output_df)
}

#' Calculate USHE Element s_36 (ACT Composite Score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_composite_score).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_36 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_36()
#'
s_36 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act = act_composite_score) %>%
    # Append USHE data element s_36
    mutate( s_36 = s_act )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_act) )
  }

  return(output_df)
}


#' Calculate USHE Element s_37 (Student's Long-term Intended CIP)
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
#' @return Original data frame, with USHE data element s_37 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_37()
#'
s_37 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_intent_cip = primary_major_cip_code) %>%
    # Append USHE data element s_37
    mutate( s_37 = s_intent_cip )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_intent_cip) )
  }

  return(output_df)
}


#' Calculate USHE Element s_38 (ACT English Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_english_score).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_38 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_38()
#'
s_38 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_eng = act_english_score) %>%
    # Append USHE data element s_38
    mutate( s_38 = s_act_eng )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_act_eng) )
  }

  return(output_df)
}

#' Calculate USHE Element s_39 (ACT Math Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_math_score).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_39 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_39()
#'
s_39 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_math = act_math_score ) %>%
    # Append USHE data element s_39
    mutate( s_39 = s_act_math )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_act_math) )
  }

  return(output_df)
}

#' Calculate USHE Element s_40 (ACT Reading Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_read_score).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_40 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_40()
#'
s_40 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_read = act_read_score ) %>%
    # Append USHE data element s_40
    mutate( s_40 = s_act_read )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_act_read) )
  }

  return(output_df)
}


#' Calculate USHE Element s_41 (ACT Science Sub-score)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (act_science_score).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_41 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_41()
#'
s_41 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_act_sci = act_science_score) %>%
    # Append USHE data element s_41
    mutate( s_41 = s_act_sci )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_act_sci) )
  }

  return(output_df)
}


#' Calculate USHE Element s_42 (High School Graduating Date)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (high_school_graduation_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_42 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_42()
#'
s_42 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_hs_grad_date = high_school_graduation_date) %>%
    # Append USHE data element s_42
    mutate( s_42 = s_hs_grad_date )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_hs_grad_date) )
  }

  return(output_df)
}

#' Calculate USHE Element s_43 (Term GPA)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (institutional_gpa).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_43 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_43()
#'
s_43 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_term_gpa = institutional_gpa ) %>%
    # Append USHE data element s_43
    mutate( s_43 = s_term_gpa )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_term_gpa) )
  }

  return(output_df)
}


#' Calculate USHE Element s_44 (Pell Indicator)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Pell Indicator
#' - FIELD NAME: S_PELL
#' - FIELD FORMAT: Varchar, 1 Character,
#' - DEFINITION: This flags the student as having received a Pell grant during the reporting semester.
#'               This field is needed for end of term extracts only.
#'               It will be accepted at 3rd week if it is convenient for the institution to provide it.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_pell_eligible, is_pell_awarded).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_44 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_44()
#'
s_44 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_pell = case_when( is_pell_eligible == TRUE ~ "E",
                               is_pell_awarded == TRUE ~ "R" )) %>%
    # Append USHE data element s_44
    mutate( s_44 = s_pell )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_pell) )
  }

  return(output_df)
}


#' Calculate USHE Element s_45 (BIA Flag)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: BIA Flag
#' - FIELD NAME: S_BIA
#' - FIELD FORMAT: Varchar, 1 Character,
#' - DEFINITION: This flags the student as having received assistance from the Bureau of Indian Affairs (BIA) during the reporting semester.
#'               This field is needed for end of term extracts only.
#'               It will be  accepted at 3rd week if it is convenient for the institution to provide it.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_bia).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_45 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_45()
#'
s_45 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_bia = if_else(is_bia, "B", "")) %>%
    # Append USHE data element s_45
    mutate( s_45 = s_bia )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_bia) )
  }

  return(output_df)
}

#' Calculate USHE Element s_46 (Major College)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_college_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_46 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_46()
#'
s_46 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_college = primary_major_college_id) %>%
    # Append USHE data element s_46
    mutate( s_46 = s_college )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_college) )
  }

  return(output_df)
}

#' Calculate USHE Element s_47 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_47 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_47()
#'
s_47 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_major = primary_major_desc) %>%
    mutate( intermediate_field_2 = "2") %>%
    # Append USHE data element s_47
    mutate( s_47 = s_major )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_major) )
  }

  return(output_df)
}

#' Calculate USHE Element s_48 (Major College for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_college_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_48 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_48()
#'
s_48 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_college2 = secondary_major_college_id ) %>%
    # Append USHE data element s_48
    mutate( s_48 = s_college2 )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_college2) )
  }

  return(output_df)
}

#' Calculate USHE Element s_49 (Major Name for Second Major)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (secondary_major_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element s_49 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' s_49()
#'
s_49 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( s_major2 = secondary_major_desc) %>%
    # Append USHE data element s_49
    mutate( s_49 = s_major2 )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(s_major2) )
  }

  return(output_df)
}
