"""Standardize a single name that is missing from the pre-built dictionary."""

import numpy as np
from typing import Optional

from ._string_distance import compute_distances


def standardize_missing_name(
    name: str,
    standard_names: "pandas.DataFrame",
    lambda_: float,
    delta: float,
    method: str = "jw",
) -> Optional[str]:
    """Return the best-guess standard name for *name* (not in the dictionary).

    Computes a posterior probability over all standard names using distance-
    weighted likelihoods.  Returns ``None`` if no candidate exceeds the
    posterior cutoff of 0.50.

    Parameters
    ----------
    name : str
        A single cleaned name string to standardize.
    standard_names : pandas.DataFrame
        Data frame with columns ``"standard"`` and ``"p_standard"``,
        containing only unique standard names (rows deduped on ``"standard"``).
        Typically a subset of the output of ``make_bayes_choice_dictionary()``.
    lambda_ : float
        Posterior mean of lambda from ``draw_gibbs()``.
    delta : float
        Posterior mean of delta from ``draw_gibbs()``.
    method : str
        String distance metric (``"jw"`` or ``"lv"``).

    Returns
    -------
    str or None
        The standardized name, or ``None`` if below the cutoff.
    """
    std = np.asarray(standard_names["standard"])
    p_std = np.asarray(standard_names["p_standard"], dtype=float)
    dp = delta * p_std

    # Length-based penalty (rewards longer, more specific standard names)
    penalty = 1.0 - np.exp(-0.8 * (np.array([len(s) for s in std]) - 1))

    # 1) Distances and threshold
    D = compute_distances(name, std, method)
    thresh = 0.15 if method == "jw" else 3.0
    D = np.where(D > thresh, np.inf, D)

    # 2) Weights and phi
    W = np.exp(-D * lambda_)
    W = np.where(np.isinf(D), 0.0, W)
    phi = W * dp

    # 3) Posterior max
    total = phi.sum()
    if total == 0:
        return None

    best_idx = int(np.argmax(phi))
    max_post = (phi[best_idx] / total) * penalty[best_idx]

    # 4) Cutoff
    if np.isnan(max_post) or max_post < 0.5:
        return None

    return str(std[best_idx])
