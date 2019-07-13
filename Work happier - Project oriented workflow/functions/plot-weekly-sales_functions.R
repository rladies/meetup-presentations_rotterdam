# This function plots the weekly sales for a country of interest
PlotWeeklySalesPerCountry <- function(country_value) {
  sales_weekly %>% 
    filter(country_code == country_value) %>% 
    
    ggplot(aes(x = date, y = count / 1000)) +
    geom_line(aes(colour = widget)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_log10() +
    labs(x = "", y = "Units / thousand") +
    ggtitle(("Weekly Sales for:"), paste(country_value))
}