# 複数の媒介変数での媒介分析
# affected_post%weight → 複数の媒介変数 → symptoms_post
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(readxl)

set.seed(12345)

input_xlsx <- "main.xlsx"
df <- read_excel(input_xlsx, sheet = 1)
names(df) <- trimws(names(df))

# 基本的な変数の定義
x_var <- "affected_post%weight"
y_var <- "symptoms _post"
covariates <- c("年齢（歳）", "sex", "BMI", "PCS_sum_pre", "ROM_flex_post")

# 試す媒介変数のリスト
mediators <- c("TUG_post", "ROM_flex_post", "PCS_sum_pre")

# データクリーニング（全変数を含める）
all_vars <- c(x_var, y_var, covariates, mediators)
df_clean <- df %>%
  mutate(across(all_of(all_vars), ~ as.numeric(.))) %>%
  drop_na(all_of(all_vars))

cat("サンプルサイズ:", nrow(df_clean), "\n\n")

# バッククォート関数
add_backticks <- function(var) {
  if (grepl("[^a-zA-Z0-9_]", var)) {
    return(paste0("`", var, "`"))
  } else {
    return(var)
  }
}

# 結果を保存するリスト
results_list <- list()

# ========== 各媒介変数について循環分析 ==========
for (m_var in mediators) {
  cat("\n\n")
  cat("============================================================\n")
  cat("媒介変数: ", m_var, "\n")
  cat("============================================================\n\n")
  
  # バッククォート付き変数名
  x_var_key <- add_backticks(x_var)
  m_var_key <- add_backticks(m_var)
  y_var_key <- add_backticks(y_var)
  covariates_key <- sapply(covariates, add_backticks)
  
  # パス a: X → M
  fmla_a <- as.formula(paste0(m_var_key, " ~ ", x_var_key, " + ",
                              paste(covariates_key, collapse = " + ")))
  model_a <- lm(fmla_a, data = df_clean)
  
  if (x_var_key %in% names(coef(model_a))) {
    a_coef <- coef(model_a)[x_var_key]
    a_ci <- confint(model_a)[x_var_key, ]
    a_pval <- summary(model_a)$coefficients[x_var_key, "Pr(>|t|)"]
    cat("a (X → M):", round(a_coef, 4), 
        "  95% CI: [", round(a_ci[1], 4), ", ", round(a_ci[2], 4), "]",
        "  p =", round(a_pval, 4), "\n")
  } else {
    cat("a (X → M): 計算不可\n")
    next
  }
  
  # パス b: M → Y (X制御)
  fmla_b <- as.formula(paste0(y_var_key, " ~ ", x_var_key, " + ", m_var_key, " + ",
                              paste(covariates_key, collapse = " + ")))
  model_b <- lm(fmla_b, data = df_clean)
  
  if (m_var_key %in% names(coef(model_b))) {
    b_coef <- coef(model_b)[m_var_key]
    b_ci <- confint(model_b)[m_var_key, ]
    b_pval <- summary(model_b)$coefficients[m_var_key, "Pr(>|t|)"]
    cat("b (M → Y):", round(b_coef, 4), 
        "  95% CI: [", round(b_ci[1], 4), ", ", round(b_ci[2], 4), "]",
        "  p =", round(b_pval, 4), "\n")
  } else {
    cat("b (M → Y): 計算不可\n")
    next
  }
  
  # パス c' (直接効果)
  if (x_var_key %in% names(coef(model_b))) {
    c_prime_coef <- coef(model_b)[x_var_key]
    c_prime_ci <- confint(model_b)[x_var_key, ]
    c_prime_pval <- summary(model_b)$coefficients[x_var_key, "Pr(>|t|)"]
    cat("c' (直接効果):", round(c_prime_coef, 4),
        "  95% CI: [", round(c_prime_ci[1], 4), ", ", round(c_prime_ci[2], 4), "]",
        "  p =", round(c_prime_pval, 4), "\n")
  }
  
  # パス c (総効果)
  fmla_c <- as.formula(paste0(y_var_key, " ~ ", x_var_key, " + ",
                              paste(covariates_key, collapse = " + ")))
  model_c <- lm(fmla_c, data = df_clean)
  
  if (x_var_key %in% names(coef(model_c))) {
    c_coef <- coef(model_c)[x_var_key]
    c_ci <- confint(model_c)[x_var_key, ]
    c_pval <- summary(model_c)$coefficients[x_var_key, "Pr(>|t|)"]
    cat("c (総効果):", round(c_coef, 4), 
        "  95% CI: [", round(c_ci[1], 4), ", ", round(c_ci[2], 4), "]",
        "  p =", round(c_pval, 4), "\n")
  }
  
  # Bootstrap による媒介効果の計算
  n_boot <- 10000
  boot_indirect <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    idx <- sample(nrow(df_clean), replace = TRUE)
    df_boot <- df_clean[idx, ]
    
    model_a_boot <- lm(fmla_a, data = df_boot)
    a_boot <- coef(model_a_boot)[x_var_key]
    
    model_b_boot <- lm(fmla_b, data = df_boot)
    b_boot <- coef(model_b_boot)[m_var_key]
    
    boot_indirect[i] <- a_boot * b_boot
  }
  
  indirect_effect <- a_coef * b_coef
  indirect_ci_lower <- quantile(boot_indirect, 0.025)
  indirect_ci_upper <- quantile(boot_indirect, 0.975)
  
  cat("\\n間接効果 (a*b):", round(indirect_effect, 4),
      "  95% CI: [", round(indirect_ci_lower, 4), ", ", round(indirect_ci_upper, 4), "]\n")
  
  # 媒介率
  if (!is.na(c_coef) && c_coef != 0) {
    prop_mediated <- indirect_effect / c_coef * 100
    cat("媒介率:", round(prop_mediated, 2), "%\n")
  } else {
    cat("媒介率: 計算不可\n")
  }
  
  # 結果を保存
  results_list[[m_var]] <- data.frame(
    Mediator = m_var,
    a_Coefficient = a_coef,
    a_CI_Lower = a_ci[1],
    a_CI_Upper = a_ci[2],
    a_Pvalue = a_pval,
    b_Coefficient = b_coef,
    b_CI_Lower = b_ci[1],
    b_CI_Upper = b_ci[2],
    b_Pvalue = b_pval,
    c_prime_Coefficient = c_prime_coef,
    c_prime_CI_Lower = c_prime_ci[1],
    c_prime_CI_Upper = c_prime_ci[2],
    c_Coefficient = c_coef,
    c_CI_Lower = c_ci[1],
    c_CI_Upper = c_ci[2],
    Indirect_Effect = indirect_effect,
    Indirect_CI_Lower = indirect_ci_lower,
    Indirect_CI_Upper = indirect_ci_upper,
    Mediation_Rate_Percent = prop_mediated,
    stringsAsFactors = FALSE
  )
}

# すべての結果を結合
results_all <- do.call(rbind, results_list)
rownames(results_all) <- NULL

# 結果を表示（比較）
cat("\n\n")
cat("============================================================\n")
cat("全媒介変数の比較\n")
cat("============================================================\n\n")
print(results_all[, c("Mediator", "Indirect_Effect", "Indirect_CI_Lower", 
                       "Indirect_CI_Upper", "Mediation_Rate_Percent")])

# CSVに保存
write_csv(results_all, "mediation_results_multiple.csv")
cat("\n結果を保存しました: mediation_results_multiple.csv\n")
