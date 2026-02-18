import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.interpolate import griddata
from mpl_toolkits.mplot3d import Axes3D

def plot_market_and_model_surface(df, grid_size=80, model_name=None):
    """
    Plot 3D interpolated surface of market IV, optionally overlaying model IV.
    
    Parameters
    ----------
    df : pd.DataFrame
        Must contain 'T', 'logM', 'IV_market', optionally 'IV_model'.
    grid_size : int
    model_name : str, optional
    """
    from scipy.interpolate import griddata
    import matplotlib.pyplot as plt
    import numpy as np

    T_vals = df["T"].values
    logM_vals = df["logM"].values
    IV_mkt = df["IV_market"].values

    Ti = np.linspace(T_vals.min(), T_vals.max(), grid_size)
    Mi = np.linspace(logM_vals.min(), logM_vals.max(), grid_size)
    Tg, Mg = np.meshgrid(Ti, Mi)

    IVsurf_mkt = griddata(np.column_stack([T_vals, logM_vals]), IV_mkt, (Tg, Mg), method="linear")

    fig = plt.figure(figsize=(12, 8))
    ax = fig.add_subplot(111, projection="3d")

    ax.plot_surface(Tg, Mg, IVsurf_mkt, cmap="viridis", alpha=0.8, edgecolor="none", label="Market")

    # Plot model surface if available
    if model_name is not None and "IV_model" in df.columns:
        IV_model = df["IV_model"].values
        IVsurf_model = griddata(np.column_stack([T_vals, logM_vals]), IV_model, (Tg, Mg), method="linear")
        ax.plot_surface(Tg, Mg, IVsurf_model, cmap="plasma", alpha=0.5, edgecolor="none", label=model_name)

    ax.set_xlabel("Time to Maturity T")
    ax.invert_xaxis()
    ax.set_ylabel("log(K/S)")
    ax.set_zlabel("Implied Volatility")
    ax.set_title("Implied Volatility Surface")
    plt.show()


def plot_smiles_market_vs_model(
    df: pd.DataFrame, 
    max_maturities: int = 12, 
    model_name: str = None
):
    """
    Plot implied volatility smiles for each maturity, optionally comparing to a model.
    
    Parameters
    ----------
    df : pd.DataFrame
        Must contain columns: 'T', 'logM', 'IV_market'. If model_name is provided, must also contain 'IV_model'.
    max_maturities : int
        Maximum number of maturities to plot.
    model_name : str, optional
        Name of the model for labeling purposes. If None, only market IV is plotted.
    """
    
    unique_T = np.sort(df["T"].unique())
    if max_maturities is not None:
        unique_T = unique_T[:max_maturities]
    
    nT = len(unique_T)
    cols = 4
    rows = int(np.ceil(nT / cols))
    
    fig, axes = plt.subplots(rows, cols, figsize=(16, 3 * rows), sharex=False, sharey=False)
    axes = axes.flatten()
    
    for idx, T in enumerate(unique_T):
        ax = axes[idx]
        sub = df[np.isclose(df["T"], T)].sort_values("logM")
        
        # Plot market IV
        ax.plot(sub["logM"], sub["IV_market"], linestyle="-", linewidth=2, label="Market")
        
        # Plot model IV if provided
        if model_name is not None and "IV_model" in sub.columns:
            ax.plot(sub["logM"], sub["IV_model"], linestyle="--", linewidth=2, label=model_name)
        
        ax.set_title(f"T = {T:.3f}")
        ax.grid(True)
        if idx % cols == 0:
            ax.set_ylabel("IV")
        ax.set_xlabel("log(K/S0)")
    
    # Remove unused subplots
    for j in range(idx + 1, len(axes)):
        fig.delaxes(axes[j])
    
    # Add shared legend
    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="upper right")
    
    fig.suptitle("Implied Volatility Smiles", fontsize=16, y=1.02)
    plt.tight_layout()
    plt.show()

# plot_smiles_market_vs_model(final_calls_clean)
def compute_iv_errors(df, model_iv_col="IV_model", market_iv_col="IV_market"):
    """
    Compute ARPE_IV and RMSE_IV and return them as a list.
    """

    model_iv = df[model_iv_col].values
    market_iv = df[market_iv_col].values

    # IV errors
    iv_errors = model_iv - market_iv

    # RMSE_IV
    rmse_iv = np.sqrt(np.nanmean(iv_errors**2))

    # ARPE_IV (avoid division by zero)
    valid = market_iv > 1e-12
    arpe_iv = np.mean(np.abs(iv_errors[valid] / market_iv[valid]))

    return [
        f"ARPE_IV: {arpe_iv:.10f}",
        f"RMSE_IV: {rmse_iv:.10f}"
    ]

def plot_market_vs_model_prices(
    df,
    model_name="Model",
    strike_col="Strike",
    maturity_col="T",
    market_col="Price_market",
    model_col="Price_model"
):
    """
    Plots market vs model call prices for each maturity (T) in df.
    """

    import matplotlib.pyplot as plt
    import numpy as np

    maturities = sorted(df[maturity_col].unique())

    fig, axes = plt.subplots(len(maturities), 1, figsize=(8, 4 * len(maturities)))

    if len(maturities) == 1:
        axes = [axes]  # make it iterable

    for ax, T in zip(axes, maturities):
        sub = df[df[maturity_col] == T].sort_values(strike_col)

        ax.plot(sub[strike_col], sub[market_col],
                "o", label=f"Market (T={T:.3f})", alpha=0.7)
        ax.plot(sub[strike_col], sub[model_col],
                "-", label=f"{model_name} (T={T:.3f})", linewidth=2)

        ax.set_xlabel("Strike")
        ax.set_ylabel("Call Price")
        ax.legend()
        ax.grid(True)

    fig.suptitle(f"{model_name} — Market vs Model VIX Call Prices", fontsize=16)
    plt.tight_layout()
    plt.show()


def plot_smiles_all_models(
    df: pd.DataFrame,
    max_maturities: int = 12,
    model_columns: dict = None
):
    """
    Plot implied volatility smiles for Market, SV, SVJ, SVJJ models.
    """

    if model_columns is None:
        model_columns = {}

    unique_T = np.sort(df["T"].unique())
    unique_T = unique_T[:max_maturities]

    nT = len(unique_T)
    cols = 4
    rows = int(np.ceil(nT / cols))

    fig, axes = plt.subplots(rows, cols, figsize=(16, 3 * rows))
    axes = axes.flatten()

    # Consistent model line styles
    styles = {
        "Market": {"linestyle": "-", "linewidth": 2, "color": "C0"},
        "SV": {"linestyle": "--", "linewidth": 1.4, "color": "C1"},
        "SVJ": {"linestyle": ":", "linewidth": 1.4, "color": "C2"},
        "SVJJ": {"linestyle": "-.", "linewidth": 1.4, "color": "C3"},
    }

    # Pre-create dummy handles for legend (clean, no duplicates)
    legend_handles = []
    for label, style in styles.items():
        line = plt.Line2D([0], [0], label=label, **style)
        legend_handles.append(line)

    # Plot panels
    for idx, T in enumerate(unique_T):
        ax = axes[idx]
        sub = df[np.isclose(df["T"], T)].sort_values("logM")

        # Market
        ax.plot(sub["logM"], sub["IV_market"], **styles["Market"])

        # Models
        for model_label, colname in model_columns.items():
            if colname in sub.columns:
                ax.plot(sub["logM"], sub[colname], **styles[model_label])

        ax.set_title(f"T = {T:.3f}")
        ax.grid(True)
        if idx % cols == 0:
            ax.set_ylabel("IV")
        ax.set_xlabel("log(K/S0)")

    # Remove unused axes
    for j in range(idx + 1, len(axes)):
        fig.delaxes(axes[j])

    # Add clean legend once
    fig.legend(
        handles=legend_handles,
        loc="upper right",
        fontsize=12,
        frameon=True
    )

    fig.suptitle("Implied Volatility Smiles: Market vs. Models", fontsize=16, y=1.02)
    plt.tight_layout()
    plt.show()


# def plot_smiles_vg_models(
#     df: pd.DataFrame,
#     max_maturities: int = 12,
# ):
#     """
#     Plot implied volatility smiles for Market vs. VG and VG-CIR models.

#     Expects the DataFrame to contain:
#         'IV_market'
#         'IV_VG'
#         'IV_VG-CIR'
#     """

#     # Define the models and their corresponding df column names
#     model_columns = {
#         "VG": "IV_VG",
#         "VG-CIR": "IV_VG-CIR"
#     }

#     unique_T = np.sort(df["T"].unique())
#     unique_T = unique_T[:max_maturities]

#     nT = len(unique_T)
#     cols = 4
#     rows = int(np.ceil(nT / cols))

#     fig, axes = plt.subplots(rows, cols, figsize=(16, 3 * rows))
#     axes = axes.flatten()

#     # Consistent styles for clean comparison
#     styles = {
#         "Market": {"linestyle": "-", "linewidth": 2, "color": "C0"},
#         "VG": {"linestyle": "--", "linewidth": 1.4, "color": "C1"},
#         "VG-CIR": {"linestyle": ":", "linewidth": 1.4, "color": "C3"},
#     }

#     # Pre-create legend handles (one per model)
#     legend_handles = []
#     for label, style in styles.items():
#         line = plt.Line2D([0], [0], label=label, **style)
#         legend_handles.append(line)

#     # Plot individual panels
#     for idx, T in enumerate(unique_T):
#         ax = axes[idx]
#         sub = df[np.isclose(df["T"], T)].sort_values("logM")

#         # Market
#         ax.plot(sub["logM"], sub["IV_market"], **styles["Market"])

#         # VG and VG-CIR models
#         for model_label, colname in model_columns.items():
#             if colname in sub.columns:
#                 ax.plot(sub["logM"], sub[colname], **styles[model_label])

#         ax.set_title(f"T = {T:.3f}")
#         ax.grid(True)
#         if idx % cols == 0:
#             ax.set_ylabel("IV")
#         ax.set_xlabel("log(K/S0)")

#     # Remove unused axes
#     for j in range(idx + 1, len(axes)):
#         fig.delaxes(axes[j])

#     # Clean legend
#     fig.legend(handles=legend_handles, loc="upper right", fontsize=12, frameon=True)

#     fig.suptitle("Implied Volatility Smiles: Market vs. VG Models", fontsize=16, y=1.02)
#     plt.tight_layout()
#     plt.show()

def plot_smiles_vg_models(
    df: pd.DataFrame,
    max_maturities: int = 12,
):
    """
    Plot implied volatility smiles for Market vs. VG and VG-CIR models.

    Expects the DataFrame to contain:
        'IV_market'
        'IV_VG_price'
        'IV_VG_iv'
        'IV_VGCIR_price'
        'IV_VGCIR_iv'
    """

    # Define the models and their corresponding df column names
    model_columns = {
        "VG (Price)": "IV_VG_price",
        "VG (IV)": "IV_VG_iv",
        "VG-CIR (Price)": "IV_VGCIR_price",
        "VG-CIR (IV)": "IV_VGCIR_iv",
    }

    unique_T = np.sort(df["T"].unique())
    unique_T = unique_T[:max_maturities]

    nT = len(unique_T)
    cols = 4
    rows = int(np.ceil(nT / cols))

    fig, axes = plt.subplots(rows, cols, figsize=(16, 3 * rows))
    axes = axes.flatten()

    # Consistent styles
    styles = {
        "Market": {"linestyle": "-", "linewidth": 2, "color": "C0"},
        "VG (Price)": {"linestyle": ":", "linewidth": 1.2, "color": "C1"},
        "VG (IV)": {"linestyle": "--", "linewidth": 1.2, "color": "C1"},
        "VG-CIR (Price)": {"linestyle": ":", "linewidth": 1.2, "color": "C3"},
        "VG-CIR (IV)": {"linestyle": "--", "linewidth": 1.2, "color": "C3"},
    }

    # Legend handles
    legend_handles = []
    for label, style in styles.items():
        line = plt.Line2D([0], [0], label=label, **style)
        legend_handles.append(line)

    # Plot panels
    for idx, T in enumerate(unique_T):
        ax = axes[idx]
        sub = df[np.isclose(df["T"], T)].sort_values("logM")

        # Market
        ax.plot(sub["logM"], sub["IV_market"], **styles["Market"])

        # All model variants
        for model_label, colname in model_columns.items():
            if colname in sub.columns:
                ax.plot(sub["logM"], sub[colname], **styles[model_label])

        ax.set_title(f"T = {T:.3f}")
        ax.grid(True)
        if idx % cols == 0:
            ax.set_ylabel("IV")
        ax.set_xlabel("log(K/S0)")

    # Remove unused axes
    for j in range(idx + 1, len(axes)):
        fig.delaxes(axes[j])

    # Legend
    fig.legend(handles=legend_handles, loc="upper right", fontsize=11, frameon=True)

    fig.suptitle(
        "Implied Volatility Smiles: Market vs. VG / VG-CIR (Price vs IV Loss)",
        fontsize=16,
        y=1.02,
    )
    plt.tight_layout()
    plt.show()
