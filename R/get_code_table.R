#' @title Get Code Table
#' @description Input an EIC code and obtain the list of profile codes in the South Coast Basin
#' @import dplyr
#' @param df Cross-referencing table
#' @param eic EIC code
#' @export


get_code_table <- function(df = atref, eic) {

<<<<<<< HEAD
  if(!(eic %in% unique(df$EIC))) {
=======
  if(eic %in% unique(df$EIC)) {
>>>>>>> 45d6e57a18a48890fb924375bdd52e2f931a3990
    stop("EIC not found")
  }

  code_table <- df %>%
    filter(EIC == eic, `Area Code` %in% sc_counties) %>%
    select(!c(a, b, c, d, zero, Comment))


  return(code_table)


}
