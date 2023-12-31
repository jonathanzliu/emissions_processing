#' @title Get Time Codes
#' @description Input an EIC code and obtain a named vector of profile codes in the South Coast Basin
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_lower
#' @importFrom dplyr select filter pull
#' @param df Cross-referencing table
#' @param eic EIC code
#' @details Note: only works when the four counties use the same profile codes for a given EIC (i.e. the four counties have to have the same temporal profile)
#' @export

get_time_codes <- function(df = atref, eic, code_table = 999) {

  if(!is.data.frame(code_table)) {

    code_table <- get_code_table(eic = eic)

  }

  time_codes <- unique(code_table$Unit) %>%
    lapply(\(x) {

      code_table %>%
        select(Unit, Code) %>%
        filter(Unit == x) %>%
        pull(Code) %>%
        unique()
    })

  names(time_codes) <- unique(code_table$Unit) %>%
    str_replace_all("ALLDAY", "HOURLY") %>%
    str_to_lower()

  time_codes <- unlist(time_codes)

  return(time_codes)


}
