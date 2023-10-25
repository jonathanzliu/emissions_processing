#' @title Get Temporal Profiles
#' @description Input an EIC code and obtain list of hourly, day of week, and monthly temporal profiles (ratios)
#' @importFrom dplyr select mutate
#' @importFrom stringr str_remove_all
#' @param df Cross-referencing table
#' @param eic EIC code
#' @details Note: only works when the four counties use the same profile codes for a given EIC (i.e. the four counties have to have the same temporal profile)
#' @export

get_profile_ratios <- function(df = atref, eic) {

  time_codes <- get_code_table(eic = eic) %>%
    rename(area_code = `Area Code`) %>%
    split(.$area_code) %>%
    lapply(get_time_codes, eic = eic, df = atref)

  ptpro_ratios <- names(ptpro_list) %>%
    lapply(\(z) {

      df <- ptpro_list[[z]]

      ratio_rows <- lapply(time_codes, \(a) {

        ratio_row <- df[which(df$Code == a[z]),] %>%
          select(!c(Code, Comment)) %>%
          t %>%
          as.data.frame() %>%
          mutate(z = 1:nrow(.))

        names(ratio_row) <- c(
          paste0(z, "_ratio"),
          z %>%
            str_remove_all("ly")
        )
        if(z == "monthly") {
          ratio_row <- ratio_row %>%
            mutate(month = month.name[month])
        } else if(z == "weekly") {
          ratio_row <- ratio_row %>%
            mutate(week = dayofweek[week]) %>%
            rename(day.of.week = week)
        }


        return(ratio_row)

      })

    })

  names(ptpro_ratios) <- names(ptpro_list)
  return(ptpro_ratios)

}
