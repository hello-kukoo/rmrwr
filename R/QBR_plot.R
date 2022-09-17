library(tidyverse)
library(gt)
library(gtExtras)
library(nflreadr)

raw_qbr <- nflreadr::load_espn_qbr(league = "nfl", season = 2020:2021,
                                   summary_type = "weekly")

qbr_df <- raw_qbr %>%
  group_by(player_id) %>%
  summarise(
    name_short = unique(name_short),
    head = unique(headshot_href),
    team_abb = last(team_abb),
    n = n(),
    qbr_mean = mean(qbr_total[1:nth(-2)]),
    qbr_sd = sd(qbr_total[1:nth(-2)]),
    qbr_last = last(qbr_total),
    max_year = max(season),
    .groups = "drop"
  ) %>%
  filter(n >= 10, max_year == 2021) %>%
  select(-player_id, -max_year) %>%
  mutate(qbr_avg = qbr_mean, .before = qbr_sd) %>%
  mutate(wk1 = qbr_last, .before = qbr_mean) %>%
  arrange(desc(qbr_mean)) %>%

  base_table <- qbr_df %>%
  arrange(desc(qbr_mean)) %>%
  gt() %>%
  gt_img_rows(head, height = 25) %>%
  gt_merge_stack(name_short, team_abb) %>%
  fmt_number(wk1:qbr_sd, decimals = 1) %>%
  gt_plt_bullet(qbr_last, qbr_avg, color = "#013369", target_color = "#D50A0A") %>%
  gt_theme_538() %>%
  gt_highlight_rows(rows = (wk1 - qbr_mean) < -32,
                    fill = "#D50A0A", alpha = 0.15, font_weight = "normal") %>%
  cols_label(name_short = "QB",
             head = "", n = "G",
             wk1 = "Wk 1",
             qbr_mean = "Avg '20",
             qbr_sd = "SD",
             qbr_last = html("<span style='color:#013369;'>Week 1 QBR</span> vs <span style='color:#D50A0A;'>2020 Avg</span>")) %>%
  tab_spanner(label = "QBR", columns = wk1:qbr_sd) %>%
  tab_header(
    "Several strong QBs struggled in Week 1, 2021",
    "Highlighted QBs dropping > 32 QBR %-points from 2020 to 2021"
  ) %>%
  tab_source_note(md("**Data**: ESPN via {nflreadr} | **Table**: @thomas_mock"))

