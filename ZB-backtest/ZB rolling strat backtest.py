"""
Project: Macro Options Backtest on 30-Year U.S. Treasury Futures (ZB)

Description
-----------
This project implements a historical backtest of a macro options strategy on
30-year U.S. Treasury bond futures (ZB). The strategy continuously maintains
long exposure to convexity by purchasing a 3-month call option on the futures,
holding it for two months, and then selling it with one month remaining to
expiry before immediately rolling into a new 3-month option.

The objective is to study the behavior of long-duration convexity across
different macro regimes, including periods of rate cuts, volatility shocks,
and tightening cycles. Performance is compared against a simple buy-and-hold
futures benchmark.

Key Modeling Components
-----------------------
- Underlying: Continuous ZB futures price series (monthly frequency)
- Option pricing: Black (1976) model for options on futures
- Volatility: Time-varying implied volatility proxy derived from the MOVE index
- Volatility skew: Simple adjustment making out-of-the-money options more expensive
- Transaction costs: Proportional premium cost applied at entry and exit
- Rolling mechanics: Single position held for two months, sold with one month remaining
- P&L tracking: Dollar P&L per contract using approximate ZB contract multiplier
- Crisis analysis: Performance evaluated during selected macro stress periods
  (2008 financial crisis, COVID shock, 2022 rate shock)

The backtest provides insight into how convexity exposure behaves relative to
linear duration exposure under different interest-rate regimes.

Limitations
-----------
This model is intended as a research prototype and contains several simplifying
assumptions:

1. Continuous futures series is used instead of specific deliverable contracts,
   which may distort option moneyness and roll behavior.
2. MOVE index is used as a proxy for implied volatility rather than actual
   historical option prices.
3. Monthly time resolution ignores intra-month price dynamics, gamma effects,
   and volatility of volatility.
4. Volatility skew is modeled with a simple linear adjustment rather than a
   full implied volatility surface.
5. Transaction costs are constant and do not vary with liquidity conditions
   or market stress.
6. Interest rate discounting is simplified.
7. Capital usage, margin requirements, and portfolio sizing are not modeled.
8. Options are treated with European pricing assumptions, whereas exchange-
   traded Treasury options may exhibit American exercise features.
9. Contract multiplier is approximated for simplicity.

Results should therefore be interpreted qualitatively rather than as exact
replications of tradable performance.

Potential Improvements
----------------------
Several extensions could substantially increase realism and analytical value:

- Use historical option settlement data instead of volatility proxies
- Implement contract-specific futures rolls aligned with option expiries
- Introduce daily mark-to-market simulation instead of monthly sampling
- Model a full implied volatility surface (strike and maturity dimensions)
- Allow regime-dependent transaction costs and liquidity assumptions
- Incorporate capital allocation and risk-targeted position sizing
- Compare single-position rolling versus constant-maturity ladder strategies
- Evaluate portfolios combining futures and options exposure
- Perform parameter sweeps over moneyness and maturity
- Calibrate volatility mapping from MOVE to observed option prices
- Include interest rate discount curves explicitly

Despite simplifications, the framework captures the core economic trade-off
between carry and convexity in long-duration fixed-income markets and provides
a useful foundation for further research.
"""

import pandas as pd
import numpy as np
from scipy.stats import norm
import matplotlib.pyplot as plt
import yfinance as yf

CONTRACT_MULTIPLIER = 1000  # $ per 1 point move

# ==========================
# Load ZB + MOVE
# ==========================

df = pd.read_csv("ZB_continuous.csv", skiprows=2)
df.columns = ["Date", "F"]

df["Date"] = pd.to_datetime(df["Date"])
df["F"] = pd.to_numeric(df["F"], errors="coerce")

df = df.dropna()
df = df[df["F"] > 0].sort_values("Date").set_index("Date")

df_m = df.resample("ME").last().dropna()

# MOVE download
move = yf.download("^MOVE", start="1990-01-01", auto_adjust=False, progress=False)

if isinstance(move.columns, pd.MultiIndex):
    move_close = move["Close"].iloc[:, 0]
else:
    move_close = move["Close"]

move = move_close.to_frame(name="MOVE").dropna()
move_m = move.resample("ME").last().dropna()

df_m = df_m.merge(move_m, left_index=True, right_index=True, how="inner")

print("Merged dataset:")
print(df_m.head())

# ==========================
# Volatility from MOVE
# ==========================

MOVE_SCALE = 0.10
SIGMA_MIN = 0.02
SIGMA_MAX = 0.40

df_m["sigma"] = (df_m["MOVE"] * MOVE_SCALE / 100.0).clip(
    lower=SIGMA_MIN, upper=SIGMA_MAX
)

print("\nSigma stats:")
print(df_m["sigma"].describe())


# ==========================
# Black pricing
# ==========================

def black_call_futures(F, K, T, sigma, r=0.0):
    if T <= 0:
        return max(F - K, 0.0)

    d1 = (np.log(F / K) + 0.5 * sigma**2 * T) / (sigma * np.sqrt(T))
    d2 = d1 - sigma * np.sqrt(T)

    return np.exp(-r * T) * (F * norm.cdf(d1) - K * norm.cdf(d2))


# ==========================
# Strategy: hold 2 months, sell with 1M left
# ==========================

def backtest_strategy(
    df_m_in,
    moneyness=0.0,
    TCOST_RATE=0.01,
    SKEW=2.0,
):

    df_use = df_m_in.dropna(subset=["F", "sigma"]).copy()

    holding = False
    entry_idx = None
    K_entry = None
    buy_cost_pts = None

    results = []

    for i in range(len(df_use)):

        date = df_use.index[i]
        F_now = float(df_use["F"].iloc[i])
        sigma_atm = float(df_use["sigma"].iloc[i])
        sigma_used = sigma_atm * (1 + SKEW * moneyness)

        pnl_pts = 0.0
        action = "HOLD"

        if not holding:

            # Buy new 3M
            K_entry = F_now * (1 + moneyness)

            buy_value = black_call_futures(F_now, K_entry, 3/12, sigma_used)
            buy_cost_pts = buy_value * (1 + TCOST_RATE)

            entry_idx = i
            holding = True
            action = "BUY"

        else:

            age = i - entry_idx

            if age == 2:
                # Sell with 1M remaining

                sell_value = black_call_futures(F_now, K_entry, 1/12, sigma_used)
                sell_proceeds = sell_value * (1 - TCOST_RATE)

                pnl_pts = sell_proceeds - buy_cost_pts
                action = "SELL_AND_ROLL"

                # Buy new immediately
                K_entry = F_now * (1 + moneyness)

                buy_value = black_call_futures(F_now, K_entry, 3/12, sigma_used)
                buy_cost_pts = buy_value * (1 + TCOST_RATE)

                entry_idx = i

        results.append({
            "Date": date,
            "F": F_now,
            "sigma": sigma_used,
            "Action": action,
            "PnL": pnl_pts
        })

    bt = pd.DataFrame(results).set_index("Date")

    # points
    bt["Equity"] = bt["PnL"].cumsum()
    bt["Peak"] = bt["Equity"].cummax()
    bt["Drawdown"] = bt["Equity"] - bt["Peak"]

    # dollars
    bt["PnL_$"] = bt["PnL"] * CONTRACT_MULTIPLIER
    bt["Equity_$"] = bt["PnL_$"].cumsum()
    bt["Peak_$"] = bt["Equity_$"].cummax()
    bt["Drawdown_$"] = bt["Equity_$"] - bt["Peak_$"]
    
    # Basic statistics
    total_pnl_pts = bt["PnL"].sum()
    total_pnl_dollars = bt["PnL_$"].sum()
    
    trades = (bt["Action"] == "SELL_AND_ROLL").sum()
    months = len(bt)
    
    avg_monthly_pnl = bt["PnL_$"].mean()
    avg_trade_pnl = total_pnl_dollars / trades if trades > 0 else np.nan
    
    monthly_vol = bt["PnL_$"].std()
    sharpe_monthly = avg_monthly_pnl / monthly_vol if monthly_vol > 0 else np.nan
    
    stats = {
        "TotalPnL_pts": total_pnl_pts,
        "TotalPnL_$": total_pnl_dollars,
        "AvgMonthlyPnL_$": avg_monthly_pnl,
        "AvgTradePnL_$": avg_trade_pnl,
        "MonthlyVol_$": monthly_vol,
        "SharpeMonthly": sharpe_monthly,
        "MaxDD_$": bt["Drawdown_$"].min(),
        "Trades": trades,
        "Months": months
    }


    return bt, stats


# ==========================
# Run strategies
# ==========================

bt_atm, stats_atm = backtest_strategy(df_m, moneyness=0.0)
bt_otm, stats_otm = backtest_strategy(df_m, moneyness=0.05)

print("\nATM stats:", stats_atm)
print("OTM stats:", stats_otm)


# ==========================
# Futures benchmark
# ==========================

futures = df_m.copy()
futures["PnL"] = futures["F"].diff()
futures = futures.loc[bt_atm.index]

futures["Equity"] = futures["PnL"].cumsum()
futures["PnL_$"] = futures["PnL"] * CONTRACT_MULTIPLIER
futures["Equity_$"] = futures["PnL_$"].cumsum()

futures["Drawdown_$"] = futures["Equity_$"] - futures["Equity_$"].cummax()

# ==========================
# Backtest results table (stats) -> DataFrame
# ==========================
results_df = pd.DataFrame([
    {"Strategy": "ATM Call", **{k: (float(v) if isinstance(v, (np.floating, float, int, np.integer)) else v) for k, v in stats_atm.items()}},
    {"Strategy": "5% OTM Call", **{k: (float(v) if isinstance(v, (np.floating, float, int, np.integer)) else v) for k, v in stats_otm.items()}},
])

results_df = results_df.set_index("Strategy")

print("\n===== Backtest Summary (per contract) =====")
print(results_df)


# ==========================
# Crisis table -> DataFrame (with periods)
# ==========================
CRISES = {
    "2008 Crisis": ("2007-10-01", "2009-03-31"),
    "COVID 2020": ("2020-02-01", "2020-06-30"),
    "Rate Shock 2022": ("2022-01-01", "2022-12-31"),
}

def crisis_metrics(bt, start, end):
    sub = bt.loc[start:end]
    if len(sub) == 0:
        return np.nan, np.nan, 0
    pnl = sub["PnL_$"].sum()
    months = len(sub)
    avg_month = pnl / months if months > 0 else np.nan
    return pnl, avg_month, months

rows = []
for name, (start, end) in CRISES.items():
    atm_pnl, atm_avg, months = crisis_metrics(bt_atm, start, end)
    otm_pnl, otm_avg, _ = crisis_metrics(bt_otm, start, end)
    fut_pnl, fut_avg, _ = crisis_metrics(futures, start, end)

    rows.append({
        "Period": name,
        "Start": start,
        "End": end,
        "Months": months,
        "ATM_$": atm_pnl,
        "ATM_avg_$": atm_avg,
        "OTM_$": otm_pnl,
        "OTM_avg_$": otm_avg,
        "Futures_$": fut_pnl,
        "Futures_avg_$": fut_avg,
    })

crisis_df = pd.DataFrame(rows).set_index("Period")

print("\n===== Crisis Performance ($ per contract) =====")
print(crisis_df)


# ==========================
# Plots
# ==========================

plt.figure(figsize=(12,6))

plt.plot(bt_atm.index, bt_atm["Equity_$"], label="ATM Call")
plt.plot(bt_otm.index, bt_otm["Equity_$"], label="5% OTM Call")
plt.plot(futures.index, futures["Equity_$"], label="Futures")

plt.legend()
plt.title("Equity ($ per contract)")
plt.show()


plt.figure(figsize=(12,4))

plt.plot(bt_atm.index, bt_atm["Drawdown_$"], label="ATM")
plt.plot(bt_otm.index, bt_otm["Drawdown_$"], label="OTM")
plt.plot(futures.index, futures["Drawdown_$"], label="Futures")

plt.legend()
plt.title("Drawdown ($)")
plt.show()
