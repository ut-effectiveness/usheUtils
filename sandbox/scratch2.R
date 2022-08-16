#' Calculate USHE Element c_54 (Course Site Type 3)
#'
#' @details
#'
#' **USHE Documentation**
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (meet_building_id_2, campus_id).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element c_54 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' c_54()
#'
c_54 <- function(input_df=usheUtils::fake_course_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( c_site_type2 = case_when(
      is.na(meet_building_id_3) == FALSE &  campus_id == "O01" | campus_id == "V01" ~ "V",
      is.na(meet_building_id_3) == "HURCTR" ~ "B80",
      is.na(meet_building_id_3) == FALSE ~ "AO1"))
      %>%
    # Append USHE data element c_54
    mutate( c_54 = c_site_type2 )

  if (!with_intermediates) {
    output_df <- output_df %>%
      # Remove fields used for intermediate calculations
      select( -c(c_site_type2) )
  }

  return(output_df)
}
