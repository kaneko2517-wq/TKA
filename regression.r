# regression_kss_pcs.R

# パッケージの読込
library(readr)
library(dplyr)
library(tidyr)    # drop_na用
library(broom)
library(readxl)
input_xlsx <- "main.xlsx"
output_csv <- "TKA術前・術後データ_scored_regression_R2.csv"

df <- read_excel(input_xlsx, sheet = 1)
names(df) <- trimws(names(df))

target <- "symptoms _post"
features <- c("年齢（歳）","sex","BMI","affected_post%weight","PCS_sum_pre","ROM_flex_post")

# TUG_post #activity_post＃satisfaction_post＃symptoms _post
# 欠落列チェック
missing_cols <- setdiff(c(target, features), names(df))
if (length(missing_cols) > 0) stop("足りない列: ", paste(missing_cols, collapse=", "))

df2 <- df %>%
  mutate(across(all_of(c(target, features)), ~ as.numeric(.))) %>%
  drop_na(all_of(c(target, features)))

fmla <- as.formula(paste0("`", target, "` ~ ",
                         paste(sprintf("`%s`", features), collapse = " + ")))
model <- lm(fmla, data = df2)
print(summary(model))

print(confint(model))

df2 <- df2 %>%
  mutate(y_pred = predict(model, newdata = df2),
         residual = .data[[target]] - y_pred)

write_csv(df2, output_csv)
cat("出力:", output_csv, "\n")

