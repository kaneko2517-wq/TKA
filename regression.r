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

<<<<<<< HEAD
target <- "symptoms _post"
features <- c("年齢（歳）","sex","BMI","affected_post%weight","PCS_sum_post","ROM_flex_post", "TUG_post")
=======
target <- "symptoms_post"
features <- c("年齢（歳）","BMI","sex","affected_post%weight","PCS_sum_post","TUG_post","ROM_flex_post")
>>>>>>> main

# TUG_post #activity_post＃satisfaction_post＃symptoms _post
# 欠落列チェック

# 欠落列チェック

missing_cols <- setdiff(c(target, features), names(df))
if (length(missing_cols) > 0) stop("足りない列: ", paste(missing_cols, collapse=", "))

df2 <- df %>%
  mutate(across(all_of(c(target, features)), ~ as.numeric(.))) %>%
  drop_na(all_of(c(target, features)))

fmla <- as.formula(paste0("`", target, "` ~ ",
                         paste(sprintf("`%s`", features), collapse = " + ")))

# ========== モデル1: 非標準化係数 ==========
cat("========== 非標準化係数 ==========\n")
model <- lm(fmla, data = df2)
print(summary(model))
print(confint(model))

# ========== モデル2: 標準化係数 ==========
cat("\n\n========== 標準化係数 ==========\n")
# 全変数を標準化
df2_std <- df2 %>%
  mutate(across(all_of(c(target, features)), ~ scale(.) %>% as.numeric()))

model_std <- lm(fmla, data = df2_std)
print(summary(model_std))
print(confint(model_std))

# ========== 標準化係数と非標準化係数の比較表 ==========
cat("\n\n========== 係数の比較 ==========\n")
coef_unstd <- coef(model)
coef_std <- coef(model_std)
ci_unstd <- confint(model)
ci_std <- confint(model_std)

comparison <- data.frame(
  Variable = names(coef_unstd),
  Unstd_Coef = coef_unstd,
  Unstd_CI_Lower = c(NA, ci_unstd[, 1]),
  Unstd_CI_Upper = c(NA, ci_unstd[, 2]),
  Std_Coef = coef_std,
  Std_CI_Lower = c(NA, ci_std[, 1]),
  Std_CI_Upper = c(NA, ci_std[, 2]),
  stringsAsFactors = FALSE
)
print(comparison)

df2 <- df2 %>%
  mutate(y_pred = predict(model, newdata = df2),
         residual = .data[[target]] - y_pred)

# 結果を出力ファイルに保存
write_csv(df2, output_csv)
cat("出力:", output_csv, "\n")

