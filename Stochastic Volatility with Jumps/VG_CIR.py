
# Import libraries
import numpy as np
import pandas as pd
from dataclasses import dataclass
from scipy.optimize import minimize
from DataHandling import implied_vol_call

S0 = 3094.04
BIG = 1e6

@dataclass
class VGParams:
    theta: float
    sigma: float
    nu: float

@dataclass
class CIRParams:
    kappa: float
    theta: float
    xi: float
    y0: float

@dataclass
class TCVGParams:
    vg: VGParams
    cir: CIRParams


# ADMISSIBILITY CONDITIONS
def vg_admissible(vg: VGParams):
    return (
        vg.sigma > 0 and
        vg.nu > 0 and
        (1 - vg.theta * vg.nu - 0.5 * vg.sigma**2 * vg.nu) > 1e-12
    )

def cir_admissible(cir: CIRParams):
    if min(cir.kappa, cir.theta, cir.xi, cir.y0) <= 0:
        return False
    if 2 * cir.kappa * cir.theta < cir.xi**2:
        return False
    return True

# VG LÉVY CUMULANT (log-mgf per unit time)
def kappa_VG(u, vg: VGParams):
    u = np.asarray(u, dtype=np.complex128)
    inside = 1 - vg.theta * vg.nu * u - 0.5 * vg.sigma**2 * vg.nu * u**2
    if np.any(np.real(inside) <= 0):
        return np.nan + 1j*np.nan
    return -(1.0 / vg.nu) * np.log(inside)

# LAPLACE TRANSFORM OF INTEGRATED CIR
def laplace_integrated_CIR(T, u, cir: CIRParams):
    T = float(T)
    if T <= 0:
        return np.ones_like(u, dtype=np.complex128)

    u = np.asarray(u, dtype=np.complex128)

    kappa, theta, xi, y0 = cir.kappa, cir.theta, cir.xi, cir.y0

    d = np.sqrt(kappa**2 + 2 * xi**2 * u + 0j)
    e_dT = np.exp(d * T)

    denom = (kappa + d) * (e_dT - 1) + 2 * d
    B = 2 * u * (e_dT - 1) / denom
    A = (2 * kappa * theta / xi**2) * np.log(
        2 * d * np.exp((kappa + d) * T / 2) / denom
    )

    return np.exp(A - B * y0)

# CHARACTERISTIC FUNCTION OF LOG-RETURN
def phi_X_T(xi, T, params: TCVGParams):
    vg, cir = params.vg, params.cir

    if not vg_admissible(vg) or not cir_admissible(cir):
        return np.nan + 1j*np.nan

    xi = np.asarray(xi, dtype=np.complex128)

    kappa1 = kappa_VG(1.0, vg)
    kappa_i = kappa_VG(1j * xi, vg)

    z = kappa_i - 1j * xi * kappa1

    # Laplace transform → pass -z
    return laplace_integrated_CIR(T, -z, cir)

# SINH–TANH GRID
def sinh_tanh_grid(N=2048, alpha=1.2, L=24.0):
    x = np.linspace(0.0, 1.0, N, endpoint=False)

    tan_term = np.tan(0.5 * np.pi * (1 - x))
    sinh_term = np.sinh(alpha * x)
    cosh_term = np.cosh(alpha * x)
    sin_term = np.sin(0.5 * np.pi * (1 - x))

    u = (L / tan_term) * sinh_term
    dudx = (
        L * alpha * cosh_term / tan_term +
        L * sinh_term * (0.5 * np.pi) / sin_term**2
    )

    return u.astype(np.complex128), dudx

# LEWIS PRICING FORMULA
def price_slice_tcvg_maturity(
    T, r, q, K_array, params, S0=S0,
    N_u=2048, alpha=1.2, L=24.0
):
    K_array = np.asarray(K_array, float)
    F = S0 * np.exp((r - q) * T)
    k_vals = np.log(K_array / F)

    u, jac = sinh_tanh_grid(N=N_u, alpha=alpha, L=L)
    u_shift = u - 0.5j

    phi_vals = phi_X_T(u_shift, T, params)
    denom = u**2 + 0.25

    exp_factor = np.exp(-1j * np.outer(k_vals, u))
    integrand = np.real(exp_factor * phi_vals / denom)

    integral = np.sum(integrand * jac, axis=1) / N_u

    prices = (
        np.exp(-q * T) * S0 *
        (1 - np.exp(k_vals / 2) * integral / np.pi)
    )

    intrinsic = np.maximum(
        S0 * np.exp(-q * T) - K_array * np.exp(-r * T), 0.0
    )
    upper = S0 * np.exp(-q * T)

    prices[(prices < intrinsic) | (prices > upper)] = np.nan
    return prices

# PRICING FULL SURFACE
def price_all_options_tcvg(df, params):
    df = df.copy()
    df["Tg"] = df["T"].round(10)
    df["rg"] = df["r"].round(12)
    df["qg"] = df["q"].round(12)

    prices = np.empty(len(df))

    for (T, r, q), idxs in df.groupby(["Tg", "rg", "qg"]).groups.items():
        K = df.loc[list(idxs), "Strike"].values
        prices[list(idxs)] = price_slice_tcvg_maturity(T, r, q, K, params)

    return prices

# CALIBRATION
def calibrate_joint_full_surface(market_df, init_vg, init_cir, loss_type):

    x0 = np.array([
        init_vg.theta, init_vg.sigma, init_vg.nu,
        init_cir.kappa, init_cir.theta, init_cir.xi, init_cir.y0
    ])

    bounds = [
        (-5, 5), (1e-4, 3), (1e-4, 5),
        (1e-4, 5), (1e-4, 5), (1e-4, 5), (1e-4, 5)
    ]

    def residuals_iv(x):
        params = TCVGParams(
            VGParams(x[0], x[1], x[2]),
            CIRParams(x[3], x[4], x[5], x[6])
        )

        if not vg_admissible(params.vg) or not cir_admissible(params.cir):
            return BIG * np.ones(len(market_df))

        prices = price_all_options_tcvg(market_df, params)
        if not np.all(np.isfinite(prices)):
            return BIG * np.ones(len(market_df))

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
                iv = BIG
            ivs.append(iv)

        res = np.asarray(ivs) - market_df["IV_market"].values
        res[~np.isfinite(res)] = BIG
        return res

    def residuals_price(x):
        params = TCVGParams(
            VGParams(x[0], x[1], x[2]),
            CIRParams(x[3], x[4], x[5], x[6])
        )

        if not vg_admissible(params.vg) or not cir_admissible(params.cir):
            return BIG * np.ones(len(market_df))

        model_prices = price_all_options_tcvg(market_df, params)
        if not np.all(np.isfinite(model_prices)):
            return BIG * np.ones(len(market_df))

        market_prices = market_df["Final_Call_Price"].values

        res = np.asarray(model_prices, dtype=float) - np.asarray(market_prices, dtype=float)
        res[~np.isfinite(res)] = BIG
        return res

    def obj(x):
        residuals = residuals_price if loss_type == "price" else residuals_iv
        r = residuals(x)
        return np.dot(r, r)

    # Step 1: Nelder–Mead
    res_nm = minimize(obj, x0, method="Nelder-Mead",
                      options={"maxiter": 50})

    # Step 2: L-BFGS-B
    res_bfgs = minimize(obj, res_nm.x, method="L-BFGS-B",
                        bounds=bounds, options={"maxiter": 50})

    xf = res_bfgs.x
    return TCVGParams(
        VGParams(xf[0], xf[1], xf[2]),
        CIRParams(xf[3], xf[4], xf[5], xf[6])
    )
