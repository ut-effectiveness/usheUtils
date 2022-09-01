#' Calculate USHE Element Institution (Institution)
#'
#' @details
#'
#' **USHE Documentation**
#' - ELEMENT NAME: Institution
#' - FIELD NAME: C_INST, S_INST, G_INST, SC_INST, M_INST, F_INST, R_INST, B_INST, & PF_INST
#' - FIELD FORMAT: Varchar, 4 Characters
#' - DEFINITION: The identification number used by the U.S. Department of Education’s Office of Postsecondary Education (OPE) to identify schools that have Program Participation Agreements (PPA) so that its students are eligible to participate in Federal Student Financial Assistance programs under Title IV regulations.
#'               This is a 6-digit number followed by a 2-digit suffix used to identify branches,  additional locations, and other entities that are part of the eligible institution.
#'               USHE will use the last 4 digits of the 6-digit number for the field value as additional locations will be specified by C-09 Line Item.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @param input_df A Data Frame. Must contain the following data fields: (no data fields required).
#'
#'
#' @return Original data frame, with USHE data element s_01 appended.
#' @export
#'
#' @examples
#' gen_ushe_inst()
#'
gen_ushe_inst <- function(input_df=usheUtils::fake_student_df) {
  gen_ushe_inst <- NULL

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( gen_ushe_inst = "3671") %>%
    # Append USHE data element s_01
    mutate( s_01 = gen_ushe_inst,
            c_01 = gen_ushe_inst,
            sc_01 = gen_ushe_inst,
            pf_01 = gen_ushe_inst,
            m_01 = gen_ushe_inst,
            b_01 = gen_ushe_inst,
            r_01 = gen_ushe_inst,
            g_01 = gen_ushe_inst )

  return(output_df)
}

#' @rdname gen_ushe_inst
#' @examples s_01()
#' @export
s_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples c_01()
#' @export
c_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples sc_01()
#' @export
sc_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples pf_01()
#' @export
pf_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples m_01()
#' @export
m_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples b_01()
#' @export
b_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples r_01()
#' @export
r_01 <- gen_ushe_inst

#' @rdname gen_ushe_inst
#' @examples g_01()
#' @export
g_01 <- gen_ushe_inst

#' Calculate USHE Elements for Names (Student Name)
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
#' @param input_df A Data Frame. Must contain the following data fields: (last_name, first_name, middle_name, name_suffix).
#'
#'
#' @return Original data frame, with USHE data element s_06 appended.
#' @export
#'
#' @examples
#' gen_ushe_name()
#'
gen_ushe_name <- function(input_df=usheUtils::fake_student_df) {

  output_df <- input_df %>%
    # Calculate intermediate fields
    mutate( last = coalesce(last_name, ''),
            first = coalesce(first_name, ''),
            middle = coalesce(middle_name, ''),
            suffix = coalesce(name_suffix, '') ) %>%
    # Append USHE data element gen_ushe_name
    mutate( gen_ushe_name = paste(last, first, middle, suffix, sep = "|")) %>%
    mutate(s_06 = gen_ushe_name,
           g_03 = gen_ushe_name)

  return(output_df)
}

#' @rdname gen_ushe_name
#' @examples s_06()
#' @export
s_06 <- gen_ushe_name

#' @rdname gen_ushe_name
#' @examples g_03()
#' @export
g_03 <- gen_ushe_name

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
