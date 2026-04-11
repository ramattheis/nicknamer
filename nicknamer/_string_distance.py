"""String distance helpers wrapping rapidfuzz (mirrors stringdist::stringdist)."""

import numpy as np


def compute_distances(query: str, candidates, method: str) -> np.ndarray:
    """Compute string distances from *query* to every element of *candidates*.

    Parameters
    ----------
    query : str
        The reference string.
    candidates : sequence of str
        Strings to compare against.
    method : str
        Distance metric.  Supported values:
        ``"jw"``  – Jaro-Winkler distance (in [0, 1], 0 = identical).
        ``"lv"``  – Levenshtein (edit) distance (non-negative integer).
    """
    from rapidfuzz.distance import Jaro, Levenshtein

    if method == "jw":
        # R's stringdist(method="jw") uses p=0 (no Winkler prefix bonus),
        # which is pure Jaro distance — use Jaro, NOT JaroWinkler.
        return np.array(
            [Jaro.normalized_distance(query, c) for c in candidates],
            dtype=float,
        )
    elif method in ("lv", "levenshtein"):
        return np.array(
            [float(Levenshtein.distance(query, c)) for c in candidates],
            dtype=float,
        )
    else:
        raise ValueError(
            f"Unsupported distance method '{method}'. Supported: 'jw', 'lv'."
        )
