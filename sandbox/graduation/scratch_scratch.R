#' Calculate USHE Element g_27 (Major Name)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Major Name
#' - FIELD NAME: g_major
#' - FIELD FORMAT: Varchar, 100 Characters
#' - DEFINITION: Name of student’s major as listed on transcript. Should be tied to element G_09 CIP CODE.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (primary_major_desc).
#' @param with_intermediates Boolean: Option to include intermediate calculated fields.
#'
#' @return Original data frame, with USHE data element g_27 appended. Will also return appended intermediate calculated fields, if option is set.
#' @export
#'
#' @examples
#' g_27()
#'
g_27 <- function(input_df=usheUtils::fake_graduation_df, with_intermediates=FALSE) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(g_major = primary_major_desc)%>%
    # Append USHE data element g_27
    mutate( g_27 = g_major  )

  return(output_df)
}
