
#' Generate Missionary Submission File
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name,
#'                                                                        first_name,
#'                                                                        middle_name,
#'                                                                        birth_date,
#'                                                                        term_end_date,
#'                                                                        student_id).
#'
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return
#' A Data Frame, with all of the USHE elements required for upload submission.
#' This will also include intermediate values, used to calculate USHE data elements, if option is set.
#'
#' @examples
#' generate_missionary_submission_file()
#'
#' @export
#'
generate_missionary_submission_file <- function(input_df=usheUtils::fake_mission_df, with_intermediates=FALSE) {

  ushe_data_elements <- c("m_01", "m_02", "m_03", "m_04",
                          "m_05", "m_06")

  output_df <- input_df %>%
    m_01(with_intermediates) %>%
    m_02(with_intermediates) %>%
    m_03(with_intermediates) %>%
    m_04(with_intermediates) %>%
    m_05(with_intermediates) %>%
    m_06(with_intermediates)


  if (!with_intermediates) {
    output_df <- output_df %>%
      dplyr::select( ushe_data_elements )
  }

  return(output_df)
}

#' Calculate USHE Element m_02 (Student Name)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr coalesce
#'
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element m_02 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' m_02()
#'
m_02 <- function(input_df=usheUtils::fake_mission_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_last = coalesce(last_name, ''),
            m_first = coalesce(first_name, ''),
            m_middle = coalesce(middle_name, '')) %>%
    # Append USHE data element m_02
    mutate( m_02 = paste(m_last, m_first, m_middle, sep='|'))

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(m_last, m_first, m_middle,) )
  }

  return(output_df)
}

#' Calculate USHE Element m_04 ()
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME:
#' - FIELD NAME: m_end_date
#' - FIELD FORMAT:
#' - DEFINITION:
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (end_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element m_04 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' m_04()
#'
m_04 <- function(input_df=usheUtils::fake_mission_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_end_date = gsub("-", "", end_date )) %>%
    # Append USHE data element m_04
    mutate( m_04 =  m_end_date  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c( m_end_date) )
  }

  return(output_df)
}
#' Calculate USHE Element m_05 ()
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME:
#' - FIELD NAME: m_start_date
#' - FIELD FORMAT:
#' - DEFINITION:
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (start_date).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element m_05 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' m_05()
#'
m_05 <- function(input_df=usheUtils::fake_mission_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( m_start_date = start_date ) %>%
    # Append USHE data element m_05
    mutate( m_05 =  m_start_date  )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c( m_start_date) )
  }

  return(output_df)
}

