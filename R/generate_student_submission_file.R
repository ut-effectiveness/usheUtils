

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
    append_column_S01() %>%
    append_column_S02() %>%
    append_column_S12() %>%
    append_column_S14() %>%
    dplyr::select("S01", "S02", "S12", "S14")

  return(output_df)
}
