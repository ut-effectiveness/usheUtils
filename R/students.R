
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
    s_12() %>%
    s_14() %>%
    dplyr::select("s_01", "s_02", "s_12", "s_14")

  return(output_df)
}


#' Append Column s_01 (Institution)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_01 appended.
#' @export
#'
#' @examples
#' s_01()
#'
s_01 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    mutate(s_01="3671")

  return(output_df)
}


#' Append Column s_02 (Year, Term, & Extract)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @param input_df
#'
#' @return Original data frame, with USHE data Element s_02 appended.
#' @export
#'
#' @examples
#' s_02()
#'
s_02 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # TODO: Implement code to generate element s_02 here.
    mutate(s_02="")

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
#'
#' @return Original data frame, with USHE data Element s_14 appended.
#' @export
#'
#' @examples
#' s_14()
#'
s_14 <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    mutate(s_14 = paste0(  if_else(is_hispanic_latino_ethnicity, "H", " "),
                           if_else(is_asian, "A", " "),
                           if_else(is_black, "B", " "),
                           if_else(is_american_indian_alaskan, "I", " "),
                           if_else(is_hawaiian_pacific_islander, "P", " "),
                           if_else(is_white, "W", " "),
                           if_else(is_international, "N", " "),
                           if_else(is_other_race, "U", " ") ) )

  return(output_df)

}
