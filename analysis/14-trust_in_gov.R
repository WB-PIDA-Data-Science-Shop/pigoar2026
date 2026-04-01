# Trust in Gov 




# plot time trends for 10 countries that have declined the most, comparing the difference between 2010 and 2023
low_trust_countries <- trust_gov_avg |>
  filter(year %in% c(2010, 2023)) |>
  pivot_wider(names_from = year, values_from = trust_gov_level,
              names_prefix = "y") |>
  filter(!is.na(y2010), !is.na(y2023)) |>
  mutate(trust_diff = y2023 - y2010) |>
  arrange(trust_diff) |>
  slice_head(n = 20) |>
  pull(country_code)

trust_gov_avg |>
  filter(country_code %in% low_trust_countries) |>
  ggplot(aes(x = year, y = trust_gov_level, color = country_code)) +
  geom_line() +
  labs(
    title = "Trust in Government over Time for Countries with Largest Decline",
    subtitle = "20 countries with largest drop from 2010 to 2023",
    x = "Year",
    y = "Average Trust in Government"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())