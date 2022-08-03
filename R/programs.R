
#' Generate Programs Validation File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (major_desc,
#'                                                                        required_credits,
#'                                                                        is_perkins,
#'                                                                        degree_id,
#'                                                                        ipeds_award_level_code,
#'                                                                        cip_code,
#'                                                                        academic_year).
#'
#' @return A Data Frame, with all of the intermediate values used to create the USHE elements required for upload submission.
#' @export
#'
#' @examples
#' generate_program_validation_file()
#'
generate_program_validation_file <- function(input_df=usheUtils::fake_program_df) {

  original_column_names <- colnames(input_df)

  output_df <- input_df %>%
    pf_01(with_intermediates = TRUE) %>%
    pf_02(with_intermediates = TRUE) %>%
    pf_03(with_intermediates = TRUE) %>%
    pf_04(with_intermediates = TRUE) %>%
    pf_05(with_intermediates = TRUE) %>%
    pf_06(with_intermediates = TRUE) %>%
    pf_07(with_intermediates = TRUE) %>%
    pf_08(with_intermediates = TRUE) %>%
    pf_09(with_intermediates = TRUE) %>%
    pf_10(with_intermediates = TRUE) %>%
    dplyr::select( -c("pf_01", "pf_02", "pf_03", "pf_04",
                      "pf_05", "pf_06", "pf_07", "pf_08",
                      "pf_09", "pf_10") ) %>%
    dplyr::select( -c(original_column_names) )

  return(output_df)
}


#' Generate Programs Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (major_desc,
#'                                                                        required_credits,
#'                                                                        is_perkins,
#'                                                                        degree_id,
#'                                                                        ipeds_award_level_code,
#'                                                                        cip_code,
#'                                                                        academic_year).
#'
#' @return A Data Frame, with all of the USHE elements required for upload submission.
#' @export
#'
#' @examples
#' generate_program_submission_file()
#'
generate_program_submission_file <- function(input_df=usheUtils::fake_program_df) {

  output_df <- input_df %>%
    pf_01() %>%
    pf_02() %>%
    pf_03() %>%
    pf_04() %>%
    pf_05() %>%
    pf_06() %>%
    pf_07() %>%
    pf_08() %>%
    pf_09() %>%
    pf_10() %>%
    dplyr::select( c("pf_01", "pf_02", "pf_03", "pf_04",
                     "pf_05", "pf_06", "pf_07", "pf_08",
                     "pf_09", "pf_10") )

  return(output_df)
}

#' Calculate USHE Element pf_02 (Academic Year)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (academic_year).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_02 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_02()
#'
pf_02 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_year = academic_year ) %>%
    # Append USHE data element pf_04
    mutate( pf_02 = pf_year  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_year) )
  }

  return(output_df)
}


#' Calculate USHE Element pf_03 (CIP Code)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (cip_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_03 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_03()
#'
pf_03 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_deg_level = cip_code ) %>%
    # Append USHE data element pf_04
    mutate( pf_03 = pf_deg_level  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_deg_level) )
  }

  return(output_df)
}

#' Calculate USHE Element pf_04 (Degree Level)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (ipeds_award_level_code).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_04 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_04()
#'
pf_04 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_deg_level = ipeds_award_level_code ) %>%
    # Append USHE data element pf_04
    mutate( pf_04 = pf_deg_level  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_deg_level) )
  }

  return(output_df)
}

#' Calculate USHE Element pf_05 (Degree Type)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (degree_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_05 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_05()
#'
pf_05 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_deg_type = if_else( degree_id == "MMFT",
                                      "MMF",
                                      degree_id ) ) %>%
    # Append USHE data element pf_05
    mutate( pf_05 = pf_deg_type )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_deg_type) )
  }

  return(output_df)
}

#' Calculate USHE Element pf_06 (Perkins)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_perkins).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_06 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_06()
#'
pf_06 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_perkins = is_perkins) %>%
    # Append USHE data element pf_06
    mutate( pf_06 = pf_perkins )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_perkins) )
  }

  return(output_df)
}


#' Calculate USHE Element pf_07 (Technical Education)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: ().
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_07 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_07()
#'
pf_07 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_te = 'N' ) %>%
    # Append USHE data element pf_07
    mutate( pf_07 = pf_te )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_te) )
  }

  return(output_df)
}


#' Calculate USHE Element pf_08 (Minimum Required Credit Hours)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (required_credits).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_08 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_08()
#'
pf_08 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_credit_hrs = required_credits ) %>%
    # Append USHE data element pf_08
    mutate( pf_08 = pf_credit_hrs )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_credit_hrs) )
  }

  return(output_df)
}


#' Calculate USHE Element pf_09 (Program Participation Agreement)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: ().
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_09 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_09()
#'
pf_09 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_something = 'N' ) %>%
    # Append USHE data element pf_09
    mutate( pf_09 = pf_something )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_something) )
  }

  return(output_df)
}

#' Calculate USHE Element pf_10 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (major_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element pf_10 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' pf_10()
#'
pf_10 <- function(input_df=usheUtils::fake_program_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( pf_major = major_desc ) %>%
    # Append USHE data element pf_10
    mutate( pf_10 = pf_major )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(pf_major) )
  }

  return(output_df)
}
