"""Construct the Bayes-optimal choice dictionary (Python port of the C++ impl)."""

import numpy as np
import pandas as pd
import scipy.sparse as sp
from typing import List
from tqdm import tqdm


def make_bayes_choice_dictionary(
    names: List[str],
    D,
    p: np.ndarray,
    delta: float,
    lambda_: float,
) -> pd.DataFrame:
    """Map each observed name to its Bayes-optimal standard choice.

    For each name *i*, computes the posterior distribution over possible
    true names (self + neighbors), then selects the unique argmax.

    This is a pure-Python/NumPy port of the original ``make_bayes_choice_dictionary_cpp``
    Rcpp/Armadillo implementation.

    Parameters
    ----------
    names : list of str
        Observed names of length K.
    D : scipy.sparse matrix
        K×K sparse distance matrix (e.g. from ``find_neighbors()``).
    p : array-like of shape (K,)
        Prior probability / frequency for each name.
    delta : float
        Garbling probability in [0, 1].
    lambda_ : float
        Distance decay parameter.

    Returns
    -------
    pandas.DataFrame with columns:
        ``observed``   – original names (length K).
        ``standard``   – Bayes-optimal standard name (or ``None`` if no unique max).
        ``p_standard`` – summed frequency of the standard name group (or ``NaN``).
    """
    if not isinstance(names, (list, np.ndarray)):
        raise TypeError("'names' must be a list of strings.")
    if not sp.issparse(D):
        raise TypeError("'D' must be a scipy sparse matrix.")
    K = len(names)
    p = np.asarray(p, dtype=float)
    if len(p) != K:
        raise ValueError("len(p) must equal len(names).")
    if D.shape != (K, K):
        raise ValueError("D must be K×K where K = len(names).")

    # Convert to CSC so we can efficiently iterate over columns of D.T
    # (i.e., rows of D — neighbors of each name)
    Dt = D.T.tocsc()

    names_arr = list(names)
    standard_out = [None] * K

    for i in tqdm(range(K), desc="make_bayes_choice_dictionary"):
        # Self-posterior
        phi_self = (1.0 - delta) * p[i]

        # Neighbors of i: non-zero entries in column i of Dt
        start = Dt.indptr[i]
        end = Dt.indptr[i + 1]
        row_inds = Dt.indices[start:end]   # neighbor indices
        values = Dt.data[start:end]        # distances to neighbors
        M_count = len(row_inds)

        # Neighbor weights (distance-decay, normalized)
        w = np.exp(-values * lambda_)
        w_sum = w.sum()
        if M_count > 0 and w_sum > 0:
            w = w / w_sum

        # Full posterior vector: [self, neighbor_0, neighbor_1, ...]
        N = 1 + M_count
        all_phi = np.zeros(N)
        all_phi[0] = phi_self
        for m in range(M_count):
            j = row_inds[m]
            all_phi[m + 1] = delta * p[j] * w[m]

        # Normalize
        tot = all_phi.sum()
        if tot > 0:
            all_phi /= tot

        # Find unique argmax
        mx = all_phi.max()
        count_max = int((all_phi == mx).sum())
        idx_max = int(np.argmax(all_phi))

        if count_max == 1 and mx > 0:
            if idx_max == 0:
                standard_out[i] = names_arr[i]
            else:
                standard_out[i] = names_arr[int(row_inds[idx_max - 1])]

    # Compute p_standard: sum of p[i] for all names mapping to each standard name
    sum_p: dict = {}
    for i in range(K):
        st = standard_out[i]
        if st is not None:
            sum_p[st] = sum_p.get(st, 0.0) + p[i]

    p_standard_out = [
        sum_p[standard_out[i]] if standard_out[i] is not None else float("nan")
        for i in range(K)
    ]

    return pd.DataFrame(
        {
            "observed": names_arr,
            "standard": standard_out,
            "p_standard": p_standard_out,
        }
    )
