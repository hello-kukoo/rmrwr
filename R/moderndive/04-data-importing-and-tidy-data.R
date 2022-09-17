library(fivethirtyeight)

head(drinks)

drinks_smaller <- drinks %>%
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>%
  select(-total_litres_of_pure_alcohol) %>%
  rename(
    beer = beer_servings,
    spirit = spirit_servings,
    wine = wine_servings
  )

descr(drinks_smaller)

drinks_smaller_tidy <- drinks_smaller %>%
  pivot_longer(!country, names_to = "type", values_to = "servings")

drinks_smaller_tidy %>%
  ggplot(aes(x = country, y = servings, fill = type)) +
    geom_col(position = "dodge")

relig_income %>%
  pivot_longer(cols = !religion, names_to = "income", values_to = "count") %>%
  ggplot(aes(x = religion, y = count, fill = income)) +
  geom_col(position = "dodge")
