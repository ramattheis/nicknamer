"""Draw a single sample from a Dirichlet distribution."""

import numpy as np
from typing import Union


def rdirichlet(alpha: Union[np.ndarray, list]) -> np.ndarray:
    """Draw a single sample from Dirichlet(alpha) using the gamma trick.

    Parameters
    ----------
    alpha : array-like of shape (K,)
        Concentration parameters (all strictly positive).

    Returns
    -------
    numpy.ndarray of shape (K,)
        A probability vector (sums to 1).
    """
    alpha = np.asarray(alpha, dtype=float)
    x = np.random.gamma(shape=alpha, scale=1.0)
    return x / x.sum()
