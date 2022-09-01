#' Calculate USHE Element Ethnicity (Ethnic Origin)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Ethnicity
#' - FIELD NAME: s_ethnic, g_ethnic
#' - FIELD FORMAT: Varchar, 8 Characters,
#' - DEFINITION: The racial and ethnic categories used to classify students.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @param input_df A Data Frame. Must contain the following data fields: (is_hispanic_latino_ethnicity, is_asian, is_black, is_american_indian_alaskan, is_hawaiian_pacific_islander, is_white, is_international, is_other_race).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_ethnicty appended.
#' @export
#'
#' @examples
#' gen_ushe_ethnicty()
#'
gen_ushe_ethnicty <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(ethnic_h = if_else(is_hispanic_latino_ethnicity, "H", ""),
           ethnic_a = if_else(is_asian, "A", ""),
           ethnic_b = if_else(is_black, "B", ""),
           ethnic_i = if_else(is_american_indian_alaskan, "I", ""),
           ethnic_p = if_else(is_hawaiian_pacific_islander, "P", ""),
           ethnic_w = if_else(is_white, "W", ""),
           ethnic_n = if_else(is_international, "N", ""),
           ethnic_u = if_else(is_other_race, "U", "") ) %>%
    mutate( gen_ushe_ethnicty_intermediate = paste(ethnic_h, ethnic_a, ethnic_b, ethnic_i, ethnic_p, ethnic_w, ethnic_n, ethnic_u, sep= "|" )) %>%
    # Append USHE data element gen_ushe_ethnicty
    mutate( gen_ushe_ethnicty = if_else(is_international, paste("", "", "", "", "", "", ethnic_n, "", sep = "|"), gen_ushe_ethnicty_intermediate) ) %>%
    mutate(s_14 = gen_ushe_ethnicty,
           g_07 = gen_ushe_ethnicty)

  return(output_df)
}

#' @rdname gen_ushe_ethnicty
#' @examples s_14()
#' @export
s_14 <- gen_ushe_ethnicty

#' @rdname gen_ushe_ethnicty
#' @examples g_07()
#' @export
g_07 <- gen_ushe_ethnicty
