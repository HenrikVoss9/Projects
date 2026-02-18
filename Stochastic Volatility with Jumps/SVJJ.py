import numpy as np
from DataHandling import implied_vol_call

# CHARACTERISTIC FUNCTION
def cf_SVJJ(u, T, eta, rho, kappa, rho_J, mu_V, theta, mu_S, sigma, Lambda, X_t, V_t):
    a = 1j * u + u**2
    b = eta * rho * 1j * u - kappa
    c = 1 - rho_J * mu_V * 1j * u

    gamma = np.sqrt(b*b + a * eta * eta)

    d = ((gamma - b) * T) / ((gamma - b) * c + a * mu_V) \
        - (2 * mu_V * a * np.log(
                1 - (((gamma + b) * c - mu_V * a)/(2 * gamma * c))
                    * (1 - np.exp(-gamma * T))
        )) / ((gamma * c)**2 - (b * c - mu_V * a)**2)

    A0 = (
        -kappa * theta * (
            ((gamma + b)/eta**2)*T +
            (2/eta**2)*np.log(1 - ((gamma + b)/(2*gamma))*(1 - np.exp(-gamma*T)))
        )
    )

    f = np.exp(mu_S * 1j*u - 0.5*sigma*sigma*u*u) * d
    mu_bar = np.exp(mu_S + 0.5*sigma*sigma)/(1 - rho_J * mu_V) - 1

    A = A0 - Lambda*T*(1 + mu_bar * 1j*u) + Lambda * f
    B = -a * (1 - np.exp(-gamma*T)) / (2*gamma - (gamma + b)*(1 - np.exp(-gamma*T)))

    return np.exp(A + B * V_t)

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
    eta, rho, kappa, rho_J, mu_V, theta, mu_S, sigma, Lambda,
    X_t, V_t,
    N=2048, alpha=1.2, L=24.0
):
    k = np.log(S / K) + (r - q)*T

    u_array, jac = sinh_tanh_grid(N=N, alpha=alpha, L=L)
    u_shifted = u_array - 0.5j

    phi_vals = cf_SVJJ(
        u_shifted, T, eta, rho, kappa, rho_J, mu_V,
        theta, mu_S, sigma, Lambda, X_t, V_t
    )

    denom = u_array**2 + 0.25
    integrand = np.real(np.exp(1j*u_array*k) * phi_vals) / denom
    weighted = integrand * jac

    integral = np.sum(weighted) * (1.0/N)

    return float(
        S*np.exp(-q*T)
        - (np.sqrt(S*K)*np.exp(-0.5*(r+q)*T))/np.pi * integral
    )

# LOSS FUNCTION FOR CALIBRATION
def loss(x, q, S, df):
    kappa, rho_J, mu_V, theta, eta, rho, v0, Lambda, mu_S, sigma = x

    X_t = np.log(S)
    V_t = v0

    Ks = df["Strike"].values
    Ts = df["T"].values
    rs = df["r"].values
    market_IV = df["IV_market"].values

    errs = []
    for K, T, r, iv_mkt in zip(Ks, Ts, rs, market_IV):

        model_price = Lewis_call_option_pricing_sinh_tan(
            S, K, T, r, q,
            eta, rho, kappa, rho_J, mu_V, theta, mu_S, sigma, Lambda,
            X_t, V_t,
            N=4096, alpha=1.2, L=24.0
        )
        iv_model = implied_vol_call(model_price, S, K, T, r, q)

        errs.append((iv_model - iv_mkt)**2)

    return float(np.nanmean(errs))
