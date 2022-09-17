# https://github.com/giocomai/ganttrify

library("ganttrify")

ganttrify(project = ganttrify::test_project,
          project_start_date = "2022-03")

ganttrify(project = ganttrify::test_project,
          spots = ganttrify::test_spots,
          project_start_date = "2022-03")

ganttrify(project = ganttrify::test_project,
          spots = ganttrify::test_spots,
          project_start_date = "2022-03",
          size_text_relative = 1.2,
          alpha_activity = 0.8,
          mark_quarters = TRUE,
          font_family = "PT Sans Narrow")

test_cn <- ganttrify::test_project
test_cn[1,2] <- "任务1"

ganttrify(project = test_cn,
          spots = ganttrify::test_spots,
          project_start_date = "2022-03",
          size_text_relative = 1.2,
          alpha_activity = 0.8,
          mark_quarters = TRUE,
          font_family = "FZSJ-SHIGMMZ")

test_36 <- ganttrify::test_project
test_36[11,4] <- 36

ganttrify(project = test_36,
          project_start_date = "2022-03",
          month_breaks = 3,
          show_vertical_lines = FALSE)

ganttrify(project = ganttrify::test_project_date_day,
          spots = ganttrify::test_spots_date_day,
          by_date = TRUE,
          exact_date = TRUE,
          size_text_relative = 1.2,
          month_number_label = FALSE)

library(knitr)

knitr::kable(ganttrify::test_project_date_day)
