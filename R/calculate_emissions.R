#' @title Get Hourly Emissions
#' @description Input an EIC code and obtain a data frame (within a list) of estimated hourly emissions, by month and day of week
#' @importFrom dplyr group_by summarize filter select left_join
#' @importFrom plyr join_all
#' @importFrom tidyr pivot_longer
#' @importFrom data.table rbindlist
#' @param df Cross-referencing table
#' @param eic EIC code
#' @details Note: only works when the four counties use the same profile codes for a given EIC (i.e. the four counties have to have the same temporal profile)
#' @export

calculate_emissions <- function(df = atref, eic) {

  ptpro_ratios <- get_profile_ratios(eic = eic)

  emissions_df <- emissions_all %>%
    lapply(\(x) {

      x2 <- x %>%
        filter(EIC == eic, FIPS_gai %in% sc_counties)

    }) %>%
    .[sapply(., \(b) nrow(b) > 0)] %>%
    lapply(\(c) {

      # select columns and summarize
      c %>%
        dplyr::select(FIPS_gai, EIC, POLL, ANN_VALUE) %>%
        rename(Emission = ANN_VALUE)

    }) %>%
    lapply(\(d) {

      pollutants <- unique(d$POLL)
      emission <- lapply(pollutants, \(e) {

        annual_emi <- d %>%
          filter(POLL == e) %>%
          select(FIPS_gai, Emission) %>%
          split(.$FIPS_gai) %>%
          lapply(\(dd) dd %>% pull(Emission)) %>%
          unlist()


        daily_emi <- annual_emi/365

        county_sums <- lapply(names(daily_emi), \(f) {

          hourly <- ptpro_ratios$hourly[[f]]
          monthly <- ptpro_ratios$monthly[[f]]
          weekly <- ptpro_ratios$weekly[[f]]

          ratio_collection <- data.frame(month = rep(month.name, each = 7*24), day.of.week = rep(dayofweek, each = 24, times = 12), hour = rep(1:24, times = 12*7)) %>%
            left_join(hourly) %>%
            left_join(monthly) %>%
            left_join(weekly) %>%
            mutate(
              emissions = annual_emi*monthly_ratio*weekly_ratio*(12*7/365)*hourly_ratio,
            )

          if(length(unique(ratio_collection$weekly_ratio) == 1)) {
            ratio_collection <- ratio_collection %>%
              unique()
          }

          return(ratio_collection)


        }) %>%
          rbindlist() %>%
          group_by(month, day.of.week, hour, hourly_ratio, monthly_ratio, weekly_ratio) %>%
          summarize(emissions = sum(emissions))

        names(county_sums)[names(county_sums) == "emissions"] <- e # adding in the weights

        return(county_sums)
      }) %>%
        plyr::join_all() %>%
        pivot_longer(cols = all_of(pollutants), names_to = "Pollutant", values_to = "Emissions")

    })

  if(length(emissions_df) == 1) {
    emissions_df <- emissions_df[[1]]
  }

  return(emissions_df)

}
