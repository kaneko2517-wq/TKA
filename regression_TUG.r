library(readr)
library(dplyr)
library(tidyr)    # drop_na用
library(broom)
library(readxl)

input_xlsx <- "main.xlsx"
output_csv <- "TKA術前・術後データ_TUG_regression_R.csv"

df <- read_excel(input_xlsx, sheet = 1)
names(df) <- trimws(names(df))

target <- "TUG_post"
features <- c("年齢（歳）","sex","BMI","affected_post%weight","PCS_sum_post","ROM_flex_post")

df2 <- df %>%
  mutate(across(all_of(c(target, features)), ~ as.numeric(.))) %>%
  drop_na(all_of(c(target, features)))

fmla <- as.formula(paste0("`", target, "` ~ ",
                         paste(sprintf("`%s`", features), collapse = " + ")))
model <- lm(fmla, data = df2)
print(summary(model))
print(confint(model))
