library(tidyverse)
library(gapminder)

# Chapter 5 ---------------------------------------------------------------

p <- ggplot(filter(gapminder, continent != "Oceania"),
            aes(x = gdpPercap, y = lifeExp))
p <- p + scale_x_log10()
p + geom_point()
p + geom_point(aes(color = continent))
p + geom_point(alpha = (1/3), size = 3) + geom_smooth(lwd = 2, se = FALSE)
p + geom_point(alpha = (1/3), size = 3) + facet_wrap(~ continent) +
                 geom_smooth(lwd = 1.5, se = FALSE)


# Chapter 6 ---------------------------------------------------------------

gapminder %>%
  filter(country == "Rwanda", year > 1979) %>%
  summary()
