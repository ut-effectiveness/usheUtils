#' Calculate USHE Element b_15 (Auxiliary Building)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Auxiliary Building
#' - FIELD NAME: b_aux
#' - FIELD FORMAT:  Numeric 1 Character.
#' - DEFINITION: Business space or other support facilities (as distinguished from primary programs of  instruction, research, and public service, and from organized activities and intercollegiate athletics)
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (building_auxiliary).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element b_15 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' b_04()
#'
b_15 <- function(input_df=usheUtils::fake_student_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( b_aux = building_auxiliary ) %>%
    # Append USHE data element b_15
    mutate( b_15 =  b_aux )

  return(output_df)
}
