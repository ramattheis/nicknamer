"""Log-likelihood of observed name counts under the garbling model."""

import numpy as np
from .make_kernel import make_kernel


def loglikelihood(
    p: np.ndarray,
    D,
    M,
    delta: float,
    lambda_: float,
    n_obs: np.ndarray,
) -> float:
    """Compute the log-likelihood of observed counts under the garbling model.

    Parameters
    ----------
    p : numpy.ndarray of shape (K,)
        True-name mixture weights.
    D : scipy.sparse matrix or numpy.ndarray
        K×K distance matrix.
    M : scipy.sparse matrix or numpy.ndarray
        K×K binary neighbor mask.
    delta : float
        Garbling probability in [0, 1].
    lambda_ : float
        Exponential decay rate (> 0).
    n_obs : numpy.ndarray of shape (K,)
        Observed counts per name.

    Returns
    -------
    float
        Log-likelihood ``sum_i n_obs_i * log(L_i)``,
        or ``-inf`` if any mixture probability is <= 0.
    """
    Kmat = make_kernel(D, M, delta, lambda_)
    L = np.asarray(Kmat @ p, dtype=float).ravel()

    if np.any(L <= 0):
        return -np.inf

    return float(np.sum(n_obs * np.log(L)))
