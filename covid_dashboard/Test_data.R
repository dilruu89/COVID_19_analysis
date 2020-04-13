data(coronavirus)


df <- coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(Country.Region == "Sri Lanka") %>%
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>% 
  spread(type,total) %>% 
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))

df_daily <- coronavirus %>%
  dplyr::filter(Country.Region == "Sri Lanka") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  spread(type,total) %>% 
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  )
df1 <- coronavirus %>% dplyr::filter(date == max(date))
Summary

Row {data-width=400}

valueBox(
  value = paste(format(sum(df$confirmed), big.mark = ","), "", sep = " "),
  caption = "Total confirmed cases",
  icon = "fas fa-user-md",
  color = confirmed_color
)


daily_confirmed <- coronavirus %>%
  dplyr::filter(type == "confirmed") %>%
  dplyr::filter(date >= "2020-02-29") %>%
  dplyr::mutate(country = Country.Region) %>%
  dplyr::group_by(date, country) %>%
  dplyr::summarise(total = sum(cases)) %>%
  dplyr::ungroup() %>%
  spread(country,total)


daily_confirmed %>%
  plotly::plot_ly() %>%
  plotly::add_trace(
    x = ~date,
    y = ~`Sri Lanka`,
    type = "scatter",
    mode = "lines+markers",
    name = "Sri Lanka")





