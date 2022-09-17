pacman::p_load(
  dplyr,
  gapminder,
  gt,
  kableExtra,
  DT,
  plotly,
  shiny
  )

gm_poland <- gapminder %>%
  filter(country == "Poland") %>%
  select(year, lifeExp)


# gt ----------------------------------------------------------------------

gm_poland %>%
  gt() %>%
  tab_header(title = "Life expectancy in Pland over time") %>%
  cols_label(year = "Year", lifeExp = "Life epectancy (years)") %>%
  tab_source_note(source_note = "Source: GapMinder dataset") %>%
  tab_style(
    style = list(cell_fill(color = "#F4F4F4")),
    locations = cells_body(columns = year)
  )


# kableExtra --------------------------------------------------------------

gm_poland %>%
  kbl(caption = "Life expectancy in Pland over time") %>%
  kable_material() %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  footnote(general = "Source: GapMinder dataset")


# DT datable --------------------------------------------------------------

DT::datatable(
  data = gm_poland,
  colnames = c("Year", "Life expectancy (years)"),
  caption = "Table shows life expectancy in Poland over time (Source: GapMinder dataset)",
  filter = "top"
)


# plotly ------------------------------------------------------------------

plot_ly(
  type = "table",
  columnwidth = c(100, 100),
  columnorder = c(0, 1),
  header = list(
    values = c("Year", "Life expectancy (years)"),
    align = c("center", "center"),
    line = list(width = 1, color = "#000000"),
    fill = list(color = c("#0099f9", "#0099F9")),
    font = list(size = 14, color = "#FFFFFF"),
    height = 40
  ),
  cells = list(
    values = rbind(gm_poland$year, gm_poland$lifeExp),
    align = c("center", "center"),
    line = list(width = 1, color = "#000000"),
    font = list(size = 12, color = "#000000")
  )
)
