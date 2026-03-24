import pandas as pd
import numpy as np

INPUT_CSV = "TKA術前・術後データ_scored2.csv"
TARGET_COL = "KSS Total score_post"
FEATURE_COLS = [
    "年齢（歳）",
    "身長（cm）",
    "BMI",
    "unaffected_post%weight",
    "TUG右回り(sec)_post",
    "PCS_sum_post",
]


def load_data(path):
    for header in [2, 0]:
        try:
            df = pd.read_csv(
                path,
                encoding="utf-8",
                engine="python",
                on_bad_lines="skip",
                header=header,
            )
            df.columns = df.columns.str.strip()
            available = set(df.columns)
            needed = {TARGET_COL} | set(FEATURE_COLS)
            if needed & available:
                print(f"[INFO] header={header} で読み込み (列数 {len(df.columns)})")
                return df
        except Exception as e:
            print(f"[WARN] header={header} で読み込み失敗: {e}")
    raise ValueError("CSVのヘッダーが見つかりませんでした。header=2 または header=0 を確認してください")


def run_regression(df, target, features):
    missing = [c for c in [target] + features if c not in df.columns]
    if missing:
        raise KeyError(
            f"必要なカラムが見つかりません: {missing}. CSV列名を確認してください。"
        )

    X = df[features].apply(pd.to_numeric, errors="coerce")
    y = pd.to_numeric(df[target], errors="coerce")

    data = pd.concat([X, y], axis=1).dropna()
    if data.empty:
        raise ValueError("回帰に利用可能なデータがありません（欠損値削除後）。")

    X_mat = data[features].to_numpy(dtype=float)
    y_vec = data[target].to_numpy(dtype=float)

    X_design = np.column_stack([np.ones(len(X_mat)), X_mat])
    params, residuals, rank, s = np.linalg.lstsq(X_design, y_vec, rcond=None)

    y_pred = X_design.dot(params)
    mse = np.mean((y_vec - y_pred) ** 2)
    ss_tot = np.sum((y_vec - np.mean(y_vec)) ** 2)
    r2 = 1 - np.sum((y_vec - y_pred) ** 2) / ss_tot if ss_tot != 0 else np.nan

    result = {
        "intercept": float(params[0]),
        "coefficients": dict(zip(features, params[1:].tolist())),
        "mse": float(mse),
        "r2": float(r2),
        "n_samples": int(len(data)),
        "rank": int(rank),
    }

    return result, data, params


def main():
    print("[INFO] 回帰分析開始");
    df = load_data(INPUT_CSV)
    result, model_data, params = run_regression(df, TARGET_COL, FEATURE_COLS)

    print("== 回帰分析結果 ==")
    print("サンプル数:", result["n_samples"])
    print("ランク:", result["rank"])
    print("切片(intercept):", result["intercept"])
    print("係数:")
    for k, v in result["coefficients"].items():
        print("  ", k, ":", v)
    print("MSE:", result["mse"])
    print("R2:", result["r2"])

    X_for_pred = model_data[FEATURE_COLS].to_numpy(dtype=float)
    y_pred = np.column_stack([np.ones(len(X_for_pred)), X_for_pred]).dot(params)
    df.loc[model_data.index, "KSS_Total_survived_pred"] = y_pred
    df.loc[model_data.index, "KSS_Total_survived_residual"] = model_data[TARGET_COL].astype(float) - y_pred

    out_file = "TKA術前・術後データ_scored_regression.csv"
    df.to_csv(out_file, index=False, encoding="utf-8")
    print("出力完了:", out_file)


if __name__ == "__main__":
    main()
