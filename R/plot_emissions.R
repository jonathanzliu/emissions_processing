#' @title Plot Hourly Emissions
#' @description Input an EIC code and obtain a facet plot of estimated hourly emissions, by month and day of week
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot geom_bar theme scale_x_continuous scale_fill_brewer xlab ylab ggtitle facet_grid theme_bw element_text
#' @importFrom scales brewer_pal
#' @param df Cross-referencing table
#' @param eic EIC code
#' @param dow Day of the week
#' @details Note: only works when the four counties use the same profile codes for a given EIC (i.e. the four counties have to have the same temporal profile). Can only plot one day of the week at a time.
#' @export

plot_emissions <- function(df = atref, eic, dow) {

  emissions <- calculate_emissions(eic = eic) %>%
    filter(day.of.week == dow) %>%
    group_by(month) %>%
    mutate(month_sum = sum(Emissions)) %>%
    filter(month_sum > 0) %>%
    select(!month_sum) %>%
    mutate(month = factor(month, levels = month.name))

  if(nrow(emissions) == 0) {
    stop("No data available")
  }

  p <- ggplot(data = emissions) +
    geom_bar(aes(x = hour, y = Emissions, fill = Pollutant), stat = "identity") +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(1,12,24)) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    xlab("Hour") +
    ylab("Emissions (tons/hour)") +
    ggtitle(paste(dow, "Emissions"), subtitle = paste("EIC:", eic)) +
    facet_grid(rows = vars(Pollutant), cols = vars(month), scales = "free_y") +
    theme_bw() +
    theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))


  return(p)
}
