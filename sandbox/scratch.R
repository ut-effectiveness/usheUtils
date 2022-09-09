#' Calculate USHE Element (Year, Term, & Extract)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Year, Term & Extract Code
#' - FIELD NAME: gen_ushe_, s_, sc_term_id & c_term_id
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
#' @param input_df A Data Frame. Must contain the following data fields: (season, academic_year_code, version_id).
#'
#'
#' @return Original data frame, with USHE data element gen_ushe_year_term_extract s_02, sc_02, c_02 appended.
#' @export
#'
#' @examples
#' gen_ushe_year_term_extract()
#'
gen_ushe_year_term_extract <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate(s_year = case_when(
      season == "Summer" ~ as.character( (as.numeric(academic_year_code) + 1) ),
      season == "Fall" ~ as.character(academic_year_code),
      season == "Spring" ~ as.character( academic_year_code ),
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
    # Append USHE data element year_term_extract
    mutate(year_term_extract = paste(year, term, extract, sep= "|") ) %>%
    mutate(gen_ushe_year_term_extract = year_term_extract,
           s_02 = year_term_extract,
           c_02 = year_term_extract,
           sc_02 = year_term_extract)

  return(output_df)
}

#' @rdname gen_ushe_year_term_extract
#' @examples s_02()
#' @export
s_02 <- gen_ushe_year_term_extract

#' @rdname gen_ushe_year_term_extract
#' @examples c_02()
#' @export
c_02 <- gen_ushe_year_term_extract

#' @rdname gen_ushe_year_term_extract
#' @examples sc_02()
#' @export
sc_02 <- gen_ushe_year_term_extract
