library(ggplot2)

df <- mtcars
df$cyl <- as.factor(df$cyl)

head(df)

get_box_stats <- function(y, upper_limit = max(df$mpg) * 1.15) {
  return(data.frame(
    y = 0.95 * upper_limit,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}

# visualization -----------------------------------------------------------

ggplot(df, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  # geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.3) +
  # coord_flip() +
  scale_fill_manual(values = c("#0099f8", "#e74c3c", "#2ecc71")) +
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9) +
  labs(
    title = "Miles per gallon among different cylinder options",
    subtitle = "Made by Appsilon",
    caption = "Source: MTCars dataset",
    x = "Number of cylinders",
    y = "Miles per gallon"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )
