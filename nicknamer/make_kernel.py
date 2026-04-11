"""Build the transition kernel matrix for the garbling model."""

import numpy as np
import scipy.sparse as sp


def make_kernel(D, M, delta: float, lambda_: float):
    """Build the K×K kernel P(obs = i | true = j).

    Construction
    ------------
    1. Compute unnormalized weight matrix ``W = M * exp(-lambda_ * D)``
       (element-wise, operating only on stored sparse values).
    2. Row-normalise ``W`` to obtain mutation probabilities ``E``.
    3. Combine with garbling probability: ``Kmat = delta * E.T + diag(1 - delta)``.

    Parameters
    ----------
    D : scipy.sparse matrix or numpy.ndarray
        K×K distance matrix between names.
    M : scipy.sparse matrix or numpy.ndarray
        K×K binary neighbor mask (1 = neighbor, 0 otherwise).
    delta : float
        Garbling probability in [0, 1].
    lambda_ : float
        Exponential decay rate for off-diagonal mutations (> 0).

    Returns
    -------
    scipy.sparse.csr_matrix or numpy.ndarray
        K×K kernel matrix; entry (i, j) = P(observed = i | true = j).
    """
    if sp.issparse(D):
        # Operate only on the stored non-zero values
        W = D.astype(float, copy=True)
        W.data = np.exp(-lambda_ * W.data)
        W = W.multiply(M)  # element-wise mask

        # Row-normalise
        row_sums = np.asarray(W.sum(axis=1), dtype=float).ravel()
        with np.errstate(divide="ignore", invalid="ignore"):
            inv_row_sums = np.where(row_sums != 0, 1.0 / row_sums, 0.0)
        E = sp.diags(inv_row_sums) @ W

        # Kmat = delta * E.T + (1-delta) * I
        K = E.shape[0]
        Kmat = (delta * E.T + sp.eye(K, format="csr") * (1.0 - delta)).tocsr()
    else:
        D = np.asarray(D, dtype=float)
        M = np.asarray(M, dtype=float)
        W = M * np.exp(-lambda_ * D)

        row_sums = W.sum(axis=1)
        with np.errstate(divide="ignore", invalid="ignore"):
            inv_row_sums = np.where(row_sums != 0, 1.0 / row_sums, 0.0)
        E = W * inv_row_sums[:, np.newaxis]

        Kmat = delta * E.T
        np.fill_diagonal(Kmat, 1.0 - delta)

    return Kmat
