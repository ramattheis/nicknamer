"""Build sparse distance (D) and neighbor-mask (M) matrices for a name list."""

import numpy as np
import scipy.sparse as sp
from typing import List, Optional, Dict, Tuple
from tqdm import tqdm
import multiprocessing
import functools

from ._string_distance import compute_distances


# ---------------------------------------------------------------------------
# Worker initializer / function for multiprocessing
# ---------------------------------------------------------------------------

_WORKER_NAMES: List[str] = []
_WORKER_METHOD: str = "jw"
_WORKER_MAX_DIST: float = 0.3
_WORKER_K: int = 0


def _init_find_worker(names, method, max_dist, K):
    global _WORKER_NAMES, _WORKER_METHOD, _WORKER_MAX_DIST, _WORKER_K
    _WORKER_NAMES = names
    _WORKER_METHOD = method
    _WORKER_MAX_DIST = max_dist
    _WORKER_K = K


def _find_worker(i: int) -> Dict:
    d = compute_distances(_WORKER_NAMES[i], _WORKER_NAMES, _WORKER_METHOD)
    sel = np.where((d <= _WORKER_MAX_DIST) & (np.arange(_WORKER_K) != i))[0]
    return {"i": np.full(len(sel), i, dtype=int), "j": sel, "d": d[sel]}


# ---------------------------------------------------------------------------
# Public function
# ---------------------------------------------------------------------------


def find_neighbors(
    names: List[str],
    method: str = "jw",
    max_dist: Optional[float] = None,
    ncores: int = 1,
) -> Dict[str, sp.spmatrix]:
    """Build sparse distance (D) and neighbor-mask (M) matrices for *names*.

    Computes, for a list of unique names:

    * ``D`` – sparse symmetric matrix of pairwise string distances for all
      neighbor pairs (distance ≤ *max_dist*).
    * ``M`` – sparse binary mask indicating which pairs are neighbors.

    Parameters
    ----------
    names : list of str
        Non-empty list of **unique** names.
    method : str
        Distance metric passed to ``stringdist`` equivalent.
        Common choices: ``"jw"`` (Jaro-Winkler), ``"lv"`` (Levenshtein).
    max_dist : float, optional
        Pairs with distance > *max_dist* are ignored.
        Defaults to ``0.3`` for similarity-based metrics (``"jw"``) and
        ``3`` for integer edit-distance metrics (``"lv"``).
    ncores : int
        Number of parallel worker processes.  Defaults to 1.

    Returns
    -------
    dict with keys ``"D"`` and ``"M"``
        Both values are ``scipy.sparse.csr_matrix`` of shape (K, K).
    """
    if not names or not isinstance(names[0], str):
        raise ValueError("`names` must be a non-empty list of strings.")
    if len(set(names)) != len(names):
        raise ValueError("`names` must not contain duplicates.")

    if max_dist is None:
        max_dist = 0.3 if method in ("jw", "cosine", "jaccard") else 3.0

    K = len(names)

    if ncores > 1:
        pool = multiprocessing.Pool(
            processes=ncores,
            initializer=_init_find_worker,
            initargs=(names, method, max_dist, K),
        )
        results = list(
            tqdm(pool.imap(_find_worker, range(K)), total=K, desc="find_neighbors")
        )
        pool.close()
        pool.join()
    else:
        _init_find_worker(names, method, max_dist, K)
        results = [
            _find_worker(i) for i in tqdm(range(K), desc="find_neighbors")
        ]

    is_ = np.concatenate([r["i"] for r in results])
    js_ = np.concatenate([r["j"] for r in results])
    ds_ = np.concatenate([r["d"] for r in results])

    D = sp.csr_matrix(
        (ds_, (is_, js_)),
        shape=(K, K),
    )
    M = sp.csr_matrix(
        (np.ones(len(is_), dtype=float), (is_, js_)),
        shape=(K, K),
    )

    return {"D": D, "M": M}
