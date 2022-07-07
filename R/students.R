
#' Generate Student Submission File
#'
#' @param input_df
#'
#' @return
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
    s_12() %>%
    s_14() %>%
    dplyr::select("s_01", "s_02", "s_03", "s_04",
                  "s_05", "s_06", "s_07",
                  "s_12", "s_14")

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
    mutate( s_id = if_else(student_ssn == "",
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
    mutate( s_id_flag = if_else(student_ssn == "",
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
    # Append USHE data element s_xx
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
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (list, of, required, data, fields).
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
    mutate( intermediate_field_1 = "1") %>%
    mutate( intermediate_field_2 = "2") %>%
    # Append USHE data element s_08
    mutate( s_08 = paste0(intermediate_field_1, intermediate_field_2) )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(intermediate_field_1, intermediate_field_2) )
  }

  return(output_df)
}


#' Append Column s_12 (Birth Date)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_12 appended.
#' @export
#'
#' @examples
#' s_12()
#'
s_12 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    mutate( s_12 = gsub("-", "", birth_date) )

  return(output_df)

}


#' Append Column S14 (Ethnic Origin)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#'
#' @param input_df
#' @param with_intermediates
#'
#' @return Original data frame, with USHE data Element s_14 appended.
#' @export
#'
#' @examples
#' s_14()
#'
s_14 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    mutate(H = if_else(is_hispanic_latino_ethnicity, "H", " "),
           A = if_else(is_asian, "A", " "),
           B = if_else(is_black, "B", " "),
           I = if_else(is_american_indian_alaskan, "I", " "),
           P = if_else(is_hawaiian_pacific_islander, "P", " "),
           W = if_else(is_white, "W", " "),
           N = if_else(is_international, "N", " "),
           U = if_else(is_other_race, "U", " ") ) %>%
    mutate(s_14 = paste0(H, A, B, I, P, W, N, U) )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # remove fields used for intermediate calculations
      select( -c(H, A, B, I, P, W, N, U) )
  }

  return(output_df)

}
