# 媒介分析：affected_post%weight → TUG → KSS（symptoms_post）
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(readxl)

set.seed(12345)

input_xlsx <- "main.xlsx"
df <- read_excel(input_xlsx, sheet = 1)
names(df) <- trimws(names(df))

# 変数の定義
x_var <- "PCS_sum_post"
# "affected_post%weight"
# "PCS_sum_post"

m_var <- "TUG_post"
y_var <- "symptoms_post"
covariates <- c("年齢（歳）", "sex", "BMI", "ROM_flex_post", "affected_post%weight")

# データクリーニング
all_vars <- c(x_var, m_var, y_var, covariates)
df_clean <- df %>%
  mutate(across(all_of(all_vars), ~ as.numeric(.))) %>%
  drop_na(all_of(all_vars))

# 標準化データの作成
df_std <- df_clean %>%
  mutate(across(all_of(all_vars), ~ scale(.) %>% as.vector()))

# バッククォートが必要な変数名をチェック（特殊文字を含む場合）
add_backticks <- function(var) {
  if (grepl("[^a-zA-Z0-9_]", var)) {
    return(paste0("`", var, "`"))
  } else {
    return(var)
  }
}

x_var_key <- add_backticks(x_var)
m_var_key <- add_backticks(m_var)
y_var_key <- add_backticks(y_var)
covariates_key <- sapply(covariates, add_backticks)

cat("サンプルサイズ:", nrow(df_clean), "\n\n")

# ========== ステップ1: パス a（X → M）==========
cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("ステップ1: X → M (affected_post%weight → TUG_post)\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")

fmla_a <- as.formula(paste0(m_var_key, " ~ ", x_var_key, " + ",
                            paste(covariates_key, collapse = " + ")))
model_a <- lm(fmla_a, data = df_clean)
summary_a <- summary(model_a)
print(summary_a)
cat("\nモデルAの係数:", names(coef(model_a)), "\n")
if (x_var_key %in% names(coef(model_a))) {
  a_coef <- coef(model_a)[x_var_key]
  a_ci <- confint(model_a)[x_var_key, ]
} else {
  stop("変数 ", x_var, " がモデルに見つかりません")
}
cat("\n係数 a:", a_coef, "\n95% CI: [", a_ci[1], ", ", a_ci[2], "]\n\n")

# 標準化モデル
model_a_std <- lm(fmla_a, data = df_std)
a_coef_std <- coef(model_a_std)[x_var_key]
cat("標準化係数 a:", a_coef_std, "\n\n")

# ========== ステップ2: パス b（M → Y, X制御）==========
cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("ステップ2: M → Y (TUG_post → symptoms_post, adjusted for X and covariates)\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")

fmla_b <- as.formula(paste0(y_var_key, " ~ ", x_var_key, " + ", m_var_key, " + ",
                            paste(covariates_key, collapse = " + ")))
model_b <- lm(fmla_b, data = df_clean)
summary_b <- summary(model_b)
print(summary_b)
cat("\nモデルBの係数:", names(coef(model_b)), "\n")
if (m_var_key %in% names(coef(model_b))) {
  b_coef <- coef(model_b)[m_var_key]
  b_ci <- confint(model_b)[m_var_key, ]
} else {
  stop("変数 ", m_var, " がモデルBに見つかりません")
}
if (x_var_key %in% names(coef(model_b))) {
  c_prime_coef <- coef(model_b)[x_var_key]  # 直接効果
  c_prime_ci <- confint(model_b)[x_var_key, ]
} else {
  c_prime_coef <- NA
  c_prime_ci <- c(NA, NA)
}

cat("\n係数 b (M効果):", b_coef, "\n95% CI: [", b_ci[1], ", ", b_ci[2], "]\n")
cat("係数 c' (直接効果):", c_prime_coef, "\n95% CI: [", c_prime_ci[1], ", ", c_prime_ci[2], "]\n\n")

# 標準化モデル
model_b_std <- lm(fmla_b, data = df_std)
b_coef_std <- coef(model_b_std)[m_var_key]
c_prime_coef_std <- coef(model_b_std)[x_var_key]
cat("標準化係数 b:", b_coef_std, "\n")
cat("標準化係数 c':", c_prime_coef_std, "\n\n")

# ========== ステップ3: パス c（X → Y, M制御なし）==========
cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("ステップ3: X → Y (総効果, TUG制御なし)\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")

fmla_c <- as.formula(paste0(y_var_key, " ~ ", x_var_key, " + ",
                            paste(covariates_key, collapse = " + ")))
model_c <- lm(fmla_c, data = df_clean)
summary_c <- summary(model_c)
print(summary_c)
cat("\nモデルCの係数:", names(coef(model_c)), "\n")
if (x_var_key %in% names(coef(model_c))) {
  c_coef <- coef(model_c)[x_var_key]
  c_ci <- confint(model_c)[x_var_key, ]
} else {
  stop("変数 ", x_var, " がモデルCに見つかりません")
}
cat("\n係数 c (総効果):", c_coef, "\n95% CI: [", c_ci[1], ", ", c_ci[2], "]\n\n")

# 標準化モデル
model_c_std <- lm(fmla_c, data = df_std)
c_coef_std <- coef(model_c_std)[x_var_key]
cat("標準化係数 c:", c_coef_std, "\n\n")

# ========== 媒介効果の計算（Bootstrap） ==========
cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("媒介効果の計算（Bootstrap法）\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")

n_boot <- 10000
boot_indirect <- numeric(n_boot)
boot_indirect_std <- numeric(n_boot)

set.seed(12345)
for (i in 1:n_boot) {
  # Bootstrapサンプル
  idx <- sample(nrow(df_clean), replace = TRUE)
  df_boot <- df_clean[idx, ]
  df_boot_std <- df_std[idx, ]
  
  # パス a の推定
  model_a_boot <- lm(fmla_a, data = df_boot)
  a_boot <- coef(model_a_boot)[x_var_key]
  model_a_boot_std <- lm(fmla_a, data = df_boot_std)
  a_boot_std <- coef(model_a_boot_std)[x_var_key]
  
  # パス b の推定
  model_b_boot <- lm(fmla_b, data = df_boot)
  b_boot <- coef(model_b_boot)[m_var_key]
  model_b_boot_std <- lm(fmla_b, data = df_boot_std)
  b_boot_std <- coef(model_b_boot_std)[m_var_key]
  
  # 間接効果
  boot_indirect[i] <- a_boot * b_boot
  boot_indirect_std[i] <- a_boot_std * b_boot_std
}

indirect_effect <- a_coef * b_coef
indirect_ci_lower <- quantile(boot_indirect, 0.025)
indirect_ci_upper <- quantile(boot_indirect, 0.975)

indirect_effect_std <- a_coef_std * b_coef_std
indirect_ci_lower_std <- quantile(boot_indirect_std, 0.025)
indirect_ci_upper_std <- quantile(boot_indirect_std, 0.975)

cat("\n媒介効果（間接効果）: ", indirect_effect, "\n")
cat("95% CI (Bootstrap): [", indirect_ci_lower, ", ", indirect_ci_upper, "]\n")
cat("SD (Bootstrap):", sd(boot_indirect), "\n")
cat("標準化媒介効果: ", indirect_effect_std, "\n")
cat("95% CI (Bootstrap): [", indirect_ci_lower_std, ", ", indirect_ci_upper_std, "]\n\n")

# ========== 媒介率（Proportion mediated） ==========
prop_mediated <- indirect_effect / c_coef * 100
prop_mediated_std <- indirect_effect_std / c_coef_std * 100

cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("要約\n")
cat(paste0(rep("=", 50), collapse = ""), "\n")
cat("総効果 (c):", c_coef, "[", c_ci[1], ", ", c_ci[2], "]\n")
cat("  標準化係数:", c_coef_std, "\n")
cat("直接効果 (c'):", c_prime_coef, "[", c_prime_ci[1], ", ", c_prime_ci[2], "]\n")
cat("  標準化係数:", c_prime_coef_std, "\n")
cat("間接効果 (a*b):", indirect_effect, "[", indirect_ci_lower, ", ", indirect_ci_upper, "]\n")
cat("  標準化係数:", indirect_effect_std, "[", indirect_ci_lower_std, ", ", indirect_ci_upper_std, "]\n")
cat("媒介率:", prop_mediated, "%\n")
cat("媒介率（標準化）:", prop_mediated_std, "%\n\n")

# 結果の保存
results <- data.frame(
  Effect = c("Total (c)", "Direct (c')", "Indirect (a*b)", "Path a (X→M)", "Path b (M→Y)"),
  Coefficient = c(c_coef, c_prime_coef, indirect_effect, a_coef, b_coef),
  Std_Coefficient = c(c_coef_std, c_prime_coef_std, indirect_effect_std, a_coef_std, b_coef_std),
  CI_Lower = c(c_ci[1], c_prime_ci[1], indirect_ci_lower, a_ci[1], b_ci[1]),
  CI_Upper = c(c_ci[2], c_prime_ci[2], indirect_ci_upper, a_ci[2], b_ci[2]),
  CI_Lower_Std = c(NA, NA, indirect_ci_lower_std, NA, NA),
  CI_Upper_Std = c(NA, NA, indirect_ci_upper_std, NA, NA)
)

write_csv(results, "mediation_results.csv")
cat("結果を保存しました: mediation_results.csv\n")
