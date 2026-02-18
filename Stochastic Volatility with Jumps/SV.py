import numpy as np
from DataHandling import implied_vol_call

#  CHARACTERISTIC FUNCTION
def cf_SV(u, T, eta, rho, kappa, theta, V_t):
    i = 1j

    a = u*u + i*u
    b = kappa - rho*eta*i*u
    d = np.sqrt(b*b + eta*eta*a)

    g = (b - d) / (b + d)

    exp_neg_dT = np.exp(-d*T)
    one_minus_gexp = 1.0 - g*exp_neg_dT
    one_minus_g = 1.0 - g

    C = (kappa*theta/eta**2) * ((b - d)*T - 2.0*np.log(one_minus_gexp/one_minus_g))
    D = ((b - d)/eta**2) * ((1.0 - exp_neg_dT)/one_minus_gexp)

    # NOTE: no exp(iu*log(S)) and no (r-q) drift term here by design
    return np.exp(C + D*V_t)

# SINH-TANH GRID
def sinh_tanh_grid(N=2048, alpha=1.2, L=24.0):
    x = np.linspace(0.0, 1.0, N, endpoint=False)
    u = (L / np.tan(0.5 * np.pi * (1 - x))) * np.sinh(alpha * x)

    tan_term = np.tan(0.5 * np.pi * (1 - x))
    sin_term = np.sin(0.5 * np.pi * (1 - x))
    cosh_term = np.cosh(alpha * x)
    sinh_term = np.sinh(alpha * x)

    dudx = (L * alpha * cosh_term / tan_term
            + L * sinh_term * (0.5*np.pi) / (sin_term**2))

    return u.astype(np.complex128), dudx

# LEWIS PRICING WITH SINH-TANH
def Lewis_call_option_pricing_sinh_tan(
    S, K, T, r, q,
    eta, rho, kappa, theta, V_t,
    N=2048, alpha=1.2, L=24.0
):
    k = np.log(S / K) + (r - q)*T

    u_array, jac = sinh_tanh_grid(N=N, alpha=alpha, L=L)
    u_shifted = u_array - 0.5j

    phi_vals = cf_SV(u_shifted, T, eta, rho, kappa, theta, V_t)

    denom = u_array**2 + 0.25
    integrand = np.real(np.exp(1j*u_array*k) * phi_vals) / denom
    weighted = integrand * jac

    integral = np.sum(weighted) * (1.0/N)

    price = (
        S*np.exp(-q*T)
        - (np.sqrt(S*K)*np.exp(-0.5*(r+q)*T))/np.pi * integral
    )

    return float(price)


# LOSS FUNCTION FOR CALIBRATION
def loss(x, q, S, df):
    # x = [kappa, theta, eta, rho, v0]
    kappa, theta, eta, rho, v0 = x
    V_t = v0

    Ks = df["Strike"].values
    Ts = df["T"].values
    rs = df["r"].values
    market_IV = df["IV_market"].values

    errs = []
    for K, T, r, iv_mkt in zip(Ks, Ts, rs, market_IV):

        model_price = Lewis_call_option_pricing_sinh_tan(
            S, K, T, r, q,
            eta=eta, rho=rho, kappa=kappa, theta=theta, V_t=V_t,
            N=4096, alpha=1.2, L=24.0
        )

        iv_model = implied_vol_call(model_price, S, K, T, r, q)
        errs.append((iv_model - iv_mkt)**2)

    return float(np.nanmean(errs))
