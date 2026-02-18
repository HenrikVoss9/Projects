# Import relevant libraries
import pandas as pd
import numpy as np
from pandas import Timestamp
from scipy.optimize import minimize
import matplotlib.pyplot as plt
from scipy.interpolate import griddata
from mpl_toolkits.mplot3d import Axes3D
from scipy.optimize import brentq

def get_data():
    # Read SPX data
    spx_options = pd.read_excel(
        "/Users/janickrommers/Desktop/Financial Enginering/SAMLET_KODE/Data20191113.xlsx",
        sheet_name="SPX options",
    )
    # date = spx_options.iloc[1, 1]
    # close = spx_options.iloc[1, 6]

    # Clean SPX options pd.df
    spx_options.columns = spx_options.iloc[4]
    spx_options = spx_options.tail(spx_options.shape[0] - 5)

    # Remove all options prices with bid price equal to 0
    spx_options = spx_options[spx_options["best_bid"] > 0]

    return spx_options


# # Read VIX data
# vix_options_and_futures = pd.read_excel(
#     "/Users/janickrommers/Desktop/Financial Enginering/Data20191113.xlsx",
#     sheet_name="VIX options and futures",
# )
# close_VIX = vix_options_and_futures.iloc[1, 6]

# # Clean VIX options pd.df
# vix_options = vix_options_and_futures
# vix_options.columns = vix_options.iloc[4]
# vix_options = vix_options.tail(vix_options.shape[0] - 5)
# vix_options = vix_options.iloc[:, :13]

# # Remove all options prices with bid price equal to 0
# vix_options = vix_options[vix_options["best_bid"] > 0]

# vix_futures = vix_options_and_futures
# vix_futures.columns = vix_futures.iloc[0]
# vix_futures = vix_futures.iloc[1:7]
# vix_futures = vix_futures.iloc[:, 14:]

######################################################################
### Inferring dividend yield and discount rates from option prices ###
######################################################################
### Create a function to back out the yield and rate from pairs of options with the same expiry
def select_pairs(df, expiry, spot, n=5):
    df_exp = df[df["exdate"] == expiry].copy()

    calls = df_exp[df_exp["cp_flag"] == "C"]
    puts  = df_exp[df_exp["cp_flag"] == "P"]

    calls["Strike"] = calls["Strike"].astype(float)
    puts["Strike"]  = puts["Strike"].astype(float)

   # Sort by absolute distance from ATM strike
    calls = calls.iloc[(calls["Strike"] - spot).abs().argsort()].head(n).reset_index(drop=True)
    puts  = puts.iloc[(puts["Strike"] - spot).abs().argsort()].head(n).reset_index(drop=True)

    # Create pairs matching by order (closest calls with closest puts)
    pairs = [(calls.iloc[i], puts.iloc[i]) for i in range(min(len(calls), len(puts)))]

    return pairs


def objective(params, all_pairs, all_T, S0):
    q = params[0]
    r_vec = params[1:]
    error = 0.0

    for i in range(len(all_pairs)):
        r = r_vec[i]
        T = all_T[i]

        call_list = [p[0] for p in all_pairs[i]]
        put_list  = [p[1] for p in all_pairs[i]]

        C = np.array([c["Mid"] for c in call_list], dtype=float)
        P = np.array([p["Mid"] for p in put_list], dtype=float)
        K = np.array([c["Strike"] for c in call_list], dtype=float)

        # Put price implied by put-call parity
        P_implied = C + K * np.exp(-r*T) - S0 * np.exp(-q*T)
        # Sum of squared errors
        error += np.sum((P - P_implied)**2)

    return error


def get_yield_and_rates():

    close = 3094.04
    spx_options = get_data()
    n_pairs = 10

    expiries = [
        "2019-12-20", "2020-01-17", "2020-02-21", "2020-03-20",
        "2020-04-17", "2020-06-19", "2020-09-18", "2020-10-16",
        "2020-11-20", "2020-12-18", "2021-06-18", "2021-12-17"
    ]
    expiries = [pd.to_datetime(e) for e in expiries]

    all_pairs = []
    all_T = []
    date = pd.Timestamp(2019, 11, 13)

    # Collect pairs and maturities exactly as before
    for expiry in expiries:
        pairs = select_pairs(spx_options, expiry, close, n=n_pairs)
        all_pairs.append(pairs)

        T = (expiry - date).days / 365
        all_T.append(T)

    all_T = np.array(all_T)

    m = len(expiries)

    # parameters: [q, r1, r2, ..., rm]
    init_q = 0.01
    init_r = [0.01]*m
    init = np.array([init_q] + init_r)

    # Upper and lower bounds for q and r
    bnds = [(-0.10, 0.10)]*(m+1)

    res = minimize(objective, init,
                   args=(all_pairs, all_T, close),
                   bounds=bnds,
                   method="L-BFGS-B")

    q_hat = res.x[0]
    r_hat = res.x[1:]

    return q_hat, r_hat, all_T

# Used later computing the average call price when both put and call for a given strike exists
def compute_final_call_price(group):
    mid_call = group.loc[group["cp_flag"] == "C", "Mid"]
    call_from_put = group.loc[group["cp_flag"] == "P", "call_from_put"]

    # If both call and put exist for this strike
    if len(group) == 2 and len(mid_call) == 1 and len(call_from_put) == 1:
        final_price = (mid_call.iloc[0] + call_from_put.iloc[0]) / 2
        group["Final_Call_Price"] = final_price
        return group

    # If only one option exists
    if not call_from_put.isna().all():
        group["Final_Call_Price"] = call_from_put.fillna(mid_call)
    else:
        group["Final_Call_Price"] = mid_call

    return group

# Check no arbitrage conditions (equation 29-31 from Kokholm 2016)
def check_for_no_arbitrage(df: pd.DataFrame):
    close = 3094.04
    final_calls = df[
    df["Final_Call_Price"]
    >= np.maximum(
        0,
        close * np.exp(-df["q"] * df["T"])
        - df["Strike"]
        * np.exp(-df["r"] * df["T"]),
        )
    ]

    keep_rows = final_calls.copy()

    for T, group in final_calls.groupby("T"):
        r = group["r"].iloc[0]
        g = group.sort_values("Strike").reset_index()

        K = g["Strike"].values
        C = g["Final_Call_Price"].values
        discount = np.exp(-r * T)

        remove_idx = set()

        for j in range(1, len(K)):
            slope = (C[j - 1] - C[j]) / (K[j] - K[j - 1])
            if not (0 <= slope <= discount):
                remove_idx.add(g.loc[j, "index"])
                remove_idx.add(g.loc[j - 1, "index"])

        for j in range(1, len(K) - 1):
            slope_left = (C[j - 1] - C[j]) / (K[j] - K[j - 1])
            slope_right = (C[j] - C[j + 1]) / (K[j + 1] - K[j])

            if slope_left - slope_right < 0:
                remove_idx.add(g.loc[j, "index"])

        keep_rows = keep_rows.drop(remove_idx, errors="ignore")

    return keep_rows.reset_index(drop=True)


def bs_call_price(S, K, T, r, q, sigma):
    """Black–Scholes call formula."""
    from numpy import log, sqrt, exp
    from scipy.stats import norm

    d1 = (np.log(S / K) + (r - q + 0.5 * sigma**2) * T) / (sigma * np.sqrt(T))
    d2 = d1 - sigma * np.sqrt(T)
    return S * np.exp(-q * T) * norm.cdf(d1) - K * np.exp(-r * T) * norm.cdf(d2)


def implied_vol_call(price, S, K, T, r, q):
    """Compute implied vol by solving BS price = market price."""
    if price < max(0, S * np.exp(-q * T) - K * np.exp(-r * T)) + 1e-12:
        return np.nan
    try:
        return brentq(
            lambda sigma: bs_call_price(S, K, T, r, q, sigma) - price, 1e-6, 5.0
        )
    except:
        return np.nan

def get_call_surface():
    spx_options = get_data()
    close = 3094.04
    date = pd.Timestamp(2019, 11, 13, 0, 0)

    # Remove wide bid-ask spreads
    spx_options_clean = spx_options[
        abs(spx_options["best_bid"] - spx_options["best_offer"]) < 5
    ]

    # Remove deep ITM/OTM (abs moneyness > 0.4)
    spx_options_clean = spx_options_clean[
        abs((spx_options_clean["Strike"] / close) - 1) <= 0.4
    ]

    # Attach yield, maturities and dividend yield
    q, r, T = get_yield_and_rates()
    spx_options_clean["exdate"] = pd.to_datetime(spx_options_clean["exdate"])
    spx_options_clean["T"] = (spx_options_clean["exdate"] - date).dt.days / 365
    r_interp = np.interp(spx_options_clean["T"], T, r)
    spx_options_clean["r"] = r_interp
    spx_options_clean["q"] = q

    # Put–call parity
    spx_options_clean.loc[spx_options_clean["cp_flag"] == "P", "call_from_put"] = (
        spx_options_clean["Mid"]
        + close * np.exp(-spx_options_clean["q"] * spx_options_clean["T"])
        - spx_options_clean["Strike"]
        * np.exp(-spx_options_clean["r"] * spx_options_clean["T"])
    )

    # Compute final call prices per (Strike, T)
    spx_options_clean = spx_options_clean.groupby(
        ["Strike", "T"], group_keys=False
    ).apply(compute_final_call_price)

    # Keep one row per strike-maturity
    spx_options_clean = (
        spx_options_clean.sort_values(["Strike", "T", "cp_flag"])
        .drop_duplicates(subset=["Strike", "T"], keep="first")
        .reset_index(drop=True)
    )

    # No arbitrage checks
    final_calls_clean = check_for_no_arbitrage(spx_options_clean)

    final_calls_clean["Strike"] = pd.to_numeric(final_calls_clean["Strike"], errors='coerce')
    final_calls_clean["logM"] = np.log(final_calls_clean["Strike"] / close)

    cols = ["Strike", "T", "r", "q", "Final_Call_Price", "logM"]
    final_calls_clean = final_calls_clean[cols]

    market_IV = []
    for i, row in final_calls_clean.iterrows():
        K = row["Strike"]
        T = row["T"]
        r = row["r"]
        q = row["q"]
        price = row["Final_Call_Price"]

        iv = implied_vol_call(price, close, K, T, r, q)
        market_IV.append(iv)

    final_calls_clean["IV_market"] = market_IV

    return final_calls_clean

def get_VIX_data():
    # Read SPX data
    vix_options = pd.read_excel(
    "/Users/janickrommers/Desktop/Financial Enginering/SAMLET_KODE/Data20191113.xlsx",
    sheet_name="VIX options and futures",
    )

    # Clean VIX options pd.df
    vix_options.columns = vix_options.iloc[4]
    vix_options = vix_options.tail(vix_options.shape[0] - 5)
    vix_options = vix_options.iloc[:, :13]

    vix_options = vix_options[vix_options["best_bid"] > 0]

    return vix_options

def get_VIX_call_surface():
    vix_options = get_VIX_data()
    close = 13
    date = pd.Timestamp(2019, 11, 13, 0, 0)

    # Keep only call options
    vix_options = vix_options[vix_options["cp_flag"] == "C"]

    # Remove wide bid-ask spreads
    vix_options_clean = vix_options[
        abs(vix_options["best_bid"] - vix_options["best_offer"]) < 1
    ]

    # Attach yield, maturities and dividend yield
    q, r, T = get_yield_and_rates()
    vix_options_clean["exdate"] = pd.to_datetime(vix_options_clean["exdate"])
    vix_options_clean["T"] = (vix_options_clean["exdate"] - date).dt.days / 365
    r_interp = np.interp(vix_options_clean["T"], T, r)
    vix_options_clean["r"] = r_interp
    vix_options_clean["q"] = q

    vix_options_clean["Strike"] = pd.to_numeric(vix_options_clean["Strike"], errors='coerce')
    vix_options_clean["logM"] = np.log(vix_options_clean["Strike"] / close)

    cols = ["Strike", "T", "r", "q", "Mid", "logM"]
    vix_options_clean = vix_options_clean[cols]

    return vix_options_clean

# test = get_VIX_call_surface()

2+2