## ----load packages-------------------------------------
library(readr)
library(here)
library(forcats)

## ----read csv file-------------------------------------
data <- read_csv(here("handling_categorical_data", "data", "analytics_raw.csv"), 
         col_types = cols_only(device = col_factor(levels = c("Desktop", "Tablet", "Mobile")), 
                               gender = col_factor(levels = c("female", "male", "NA")), 
                               user_rating = col_factor(levels = c("1", "2", "3", "4", "5"),
                                                        ordered = TRUE)))


## ----read RDS file-------------------------------------
# data <- readRDS(here("handling_categorical_data", "data", "analytics.rds"))
head(data)

device <- sample(c("Desktop", "Mobile", "Tablet", NA), size = 25, replace = TRUE)

# as_factor(device)
# as.factor(device)

factor(device, exclude = NULL,
        levels = c("Desktop", "Mobile", "Tablet"),
        labels = c("Desk", "Mob", "Tab", NA)
       )


rating <- sample(c("Dislike", "Neutral", "Like"), size = 25, replace = TRUE)
rating
factor(rating, levels = c("Dislike", "Neutral", "Like"), ordered = TRUE)
