library(gtsummary)
library(gt)

trial2 <- trial %>% select(trt, age, grade)

trial2 %>%
  tbl_summary(by = trt) %>%
  add_p()

trial2 %>%
  tbl_summary(
    by = trt,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c("{N_nonmiss}",
                           "{mean} ({sd})",
                           "{median} ({p25}, {p75})",
                           "{min}, {max}"),
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      grade ~ "肿瘤分期",
      age ~ "年龄"),
    missing_text = "(Missing)"
  ) %>%
  add_overall() %>%
  add_difference() %>%
  modify_header(label ~ "") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") %>%
  bold_labels()

library(dplyr)
# paired t-test -------------------------------------------------------------
# imagine that each patient received Drug A and Drug B (adding ID showing their paired measurements)
trial_paired <-
  trial %>%
  select(trt, marker, response) %>%
  group_by(trt) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  arrange(id)

# you must first delete incomplete pairs from the data, then you can build the table
trial_paired %>%
  # delete missing values
  filter(complete.cases(.)) %>%
  # keep IDs with both measurements
  group_by(id) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  # summarize data
  tbl_summary(by = trt, include = -id) %>%
  add_p(test = list(marker ~ "paired.t.test",
                    response ~ "mcnemar.test"),
        group = id)

# table summarizing data with no p-values
small_trial <- trial %>% select(grade, age, response)

t0 <- small_trial %>%
  tbl_summary(by = grade, missing = "no") %>%
  modify_header(all_stat_cols() ~ "**{level}**")

# table comparing grade I and II
t1 <- small_trial %>%
  filter(grade %in% c("I", "II")) %>%
  tbl_summary(by = grade, missing = "no") %>%
  add_p() %>%
  modify_header(p.value ~ md("**I vs. II**")) %>%
  # hide summary stat columns
  modify_column_hide(all_stat_cols())

# table comparing grade I and II
t2 <- small_trial %>%
  filter(grade %in% c("I", "III")) %>%
  tbl_summary(by = grade, missing = "no") %>%
  add_p()  %>%
  modify_header(p.value ~ md("**I vs. III**")) %>%
  # hide summary stat columns
  modify_column_hide(all_stat_cols())

# merging the 3 tables together, and adding additional gt formatting
tbl_merge(list(t0, t1, t2)) %>%
  modify_spanning_header(
    list(
      all_stat_cols() ~ "**Tumor Grade**",
      starts_with("p.value") ~ "**p-values**"
    )
  )

