

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
generate_student_submission_file <- function(input_df=fake_student_df) {

  output_df <- input_df %>%
    s_01() %>%
    s_02() %>%
    s_12() %>%
    s_14() %>%
    dplyr::select("s_01", "s_02", "s_12", "s_14")

  return(output_df)
}
