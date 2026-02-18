import numpy as np
import pandas as pd
from scipy.integrate import trapezoid
from scipy.optimize import minimize
from dataclasses import dataclass

from DataHandling import (
    get_call_surface,
    implied_vol_call,
)

S0 = 3094.04
BIG_PENALTY = 1e6

@dataclass
class VGParams:
    theta: float
    sigma: float
    nu: float

# VG ADMISSIBILITY
def vg_admissible(vg: VGParams):
    if vg.sigma <= 0 or vg.nu <= 0:
        return False
    return (1 - vg.theta * vg.nu - 0.5 * vg.sigma**2 * vg.nu) > 1e-12

# VG LÉVY CUMULANT (LOG-MGF)
def kappa_VG(u: complex, p: VGParams):
    theta, sigma, nu = p.theta, p.sigma, p.nu
    u = np.asarray(u, dtype=np.complex128)

    inside = 1 - theta * nu * u - 0.5 * sigma**2 * nu * u**2

    if np.any(np.real(inside) <= 0):
        return np.nan + 1j * np.nan

    return -(1.0 / nu) * np.log(inside)

# CF OF DRIFT-CORRECTED LOG-RETURN
def phi_X_T(xi, T, vg: VGParams):
    if T <= 0:
        return np.ones_like(xi, dtype=np.complex128)

    if not vg_admissible(vg):
        return np.nan + 1j * np.nan

    kappa1 = kappa_VG(1.0, vg)
    xi = np.asarray(xi, dtype=np.complex128)

    i_xi = 1j * xi
    kappa_i = kappa_VG(i_xi, vg)

    z = kappa_i - i_xi * kappa1
    return np.exp(T * z)

# SINH–TANH GRID
def sinh_tanh_grid(N=2048, alpha=1.2, L=24.0):
    x = np.linspace(0.0, 1.0, N, endpoint=False)

    tan_term = np.tan(0.5 * np.pi * (1 - x))
    sinh_term = np.sinh(alpha * x)
    cosh_term = np.cosh(alpha * x)
    sin_term = np.sin(0.5 * np.pi * (1 - x))

    u = (L / tan_term) * sinh_term

    dudx = (
        L * alpha * cosh_term / tan_term
        + L * sinh_term * (0.5 * np.pi) / (sin_term**2)
    )

    return u.astype(np.complex128), dudx

# LEWIS PRICING FORMULA
def price_slice_vg_maturity(
    T, r, q, K_array, vg: VGParams, S0=S0,
    N_u=2048, alpha=1.2, L=24.0
):
    if not vg_admissible(vg):
        return np.full(len(K_array), np.nan)

    K_array = np.asarray(K_array, float)

    F = S0 * np.exp((r - q) * T)
    k_vals = np.log(K_array / F)

    # sinh–tanh grid
    u, jac = sinh_tanh_grid(N=N_u, alpha=alpha, L=L)
    u_shifted = u - 0.5j

    phi_vals = phi_X_T(u_shifted, T, vg)

    denom = u**2 + 0.25
    exp_factor = np.exp(-1j * np.outer(k_vals, u))
    integrand = np.real(exp_factor * phi_vals / denom)

    integral = np.sum(integrand * jac, axis=1) / N_u

    prices = (
        np.exp(-q * T)
        * S0
        * (1.0 - np.exp(k_vals / 2.0) * integral / np.pi)
    )

    # no-arbitrage guards
    intrinsic = np.maximum(S0 * np.exp(-q * T) - K_array * np.exp(-r * T), 0.0)
    upper = S0 * np.exp(-q * T)

    prices[(prices < intrinsic) | (prices > upper)] = np.nan
    return prices

# PRICING ALL OPTIONS
def price_all_options_vg(df, vg: VGParams):
    df = df.copy()

    df["Tg"] = df["T"].round(10)
    df["rg"] = df["r"].round(12)
    df["qg"] = df["q"].round(12)

    prices = np.empty(len(df))

    for (T, r, q), idxs in df.groupby(["Tg", "rg", "qg"]).groups.items():
        idxs = list(idxs)
        K_vals = df.loc[idxs, "Strike"].values

        prices[idxs] = price_slice_vg_maturity(
            T, r, q, K_vals, vg
        )

    return prices


# MARKET DATA PREPARATION
def prepare_market_dataframe_for_calib():
    df = get_call_surface().copy()
    df["Strike"] = pd.to_numeric(df["Strike"], errors="coerce")
    df = df.dropna(subset=["Strike"]).reset_index(drop=True)

    IVs = []
    for _, row in df.iterrows():
        try:
            iv = implied_vol_call(
                row["Final_Call_Price"],
                S0,
                row["Strike"],
                row["T"],
                row["r"],
                row["q"],
            )
        except:
            iv = np.nan
        IVs.append(iv)

    df["IV_market"] = IVs
    return df.dropna(subset=["IV_market"]).reset_index(drop=True)

# CALIBRATION
def calibrate_vg_full_surface(market_df, init_vg, loss_type="price"):

    print(f"Calibrating VG model on {len(market_df)} options")

    x0 = np.array([init_vg.theta, init_vg.sigma, init_vg.nu])

    bounds = [
        (-5.0, 5.0),    # theta
        (1e-4, 3.0),    # sigma
        (1e-4, 5.0),    # nu
    ]

    def residuals_iv(x):
        vg = VGParams(x[0], x[1], x[2])

        if not vg_admissible(vg):
            return BIG_PENALTY * np.ones(len(market_df))

        prices = price_all_options_vg(market_df, vg)
        if not np.all(np.isfinite(prices)):
            return BIG_PENALTY * np.ones(len(market_df))

        ivs = []
        for price, (_, row) in zip(prices, market_df.iterrows()):
            try:
                iv = implied_vol_call(
                    price,
                    S0,
                    row["Strike"],
                    row["T"],
                    row["r"],
                    row["q"],
                )
            except:
                iv = BIG_PENALTY
            ivs.append(iv)

        res = np.asarray(ivs) - market_df["IV_market"].values
        res[~np.isfinite(res)] = BIG_PENALTY
        
        return res


    def residuals_price(x):
        vg = VGParams(x[0], x[1], x[2])

        if not vg_admissible(vg):
            return BIG_PENALTY * np.ones(len(market_df))

        model_prices = price_all_options_vg(market_df, vg)
        if not np.all(np.isfinite(model_prices)):
            return BIG_PENALTY * np.ones(len(market_df))

        market_prices = market_df["Final_Call_Price"].values

        res = np.asarray(model_prices, dtype=float) - np.asarray(market_prices, dtype=float)
        res[~np.isfinite(res)] = BIG_PENALTY
        
        return res


    def obj(x):
        residuals = residuals_price if loss_type == "price" else residuals_iv
        r = residuals(x)
        return np.dot(r, r)

    # Step 1: Nelder–Mead
    res_nm = minimize(
        obj,
        x0,
        method="Nelder-Mead",
        options={"maxiter": 50},
    )

    # Step 2: L-BFGS-B
    res_bfgs = minimize(
        obj,
        res_nm.x,
        method="L-BFGS-B",
        bounds=bounds,
        options={"maxiter": 50},
    )

    xf = res_bfgs.x
    vg_final = VGParams(xf[0], xf[1], xf[2])

    print("\nFinal VG parameters:", vg_final)
    return vg_final
