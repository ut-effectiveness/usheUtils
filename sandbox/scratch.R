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
