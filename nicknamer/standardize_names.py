"""Return Bayes-optimal standardized names for a vector of observed names."""

import multiprocessing
import warnings
from typing import List, Optional

import numpy as np
import pandas as pd
from tqdm import tqdm

from .standardize_missing_name import standardize_missing_name


# ---------------------------------------------------------------------------
# Worker setup for parallel standardisation of out-of-dictionary names
# ---------------------------------------------------------------------------

_STD_NAMES_DF = None
_STD_LAMBDA = None
_STD_DELTA = None
_STD_METHOD = None


def _init_std_worker(std_names_df, lambda_, delta, method):
    global _STD_NAMES_DF, _STD_LAMBDA, _STD_DELTA, _STD_METHOD
    _STD_NAMES_DF = std_names_df
    _STD_LAMBDA = lambda_
    _STD_DELTA = delta
    _STD_METHOD = method


def _std_worker(name: str) -> Optional[str]:
    return standardize_missing_name(name, _STD_NAMES_DF, _STD_LAMBDA, _STD_DELTA, _STD_METHOD)


# ---------------------------------------------------------------------------
# Public function
# ---------------------------------------------------------------------------


def standardize_names(
    names: List[str],
    dictionary: pd.DataFrame,
    lambda_: float,
    delta: float,
    method: str = "jw",
    ncores: int = 1,
) -> List[Optional[str]]:
    """Return Bayes-optimal standard names for a list of observed names.

    Names already present in *dictionary* are looked up directly.  Names
    missing from *dictionary* are standardised on-the-fly via
    ``standardize_missing_name()``, optionally in parallel.

    Parameters
    ----------
    names : list of str
        Cleaned observed name strings (e.g. output of ``clean_surnames()``).
    dictionary : pandas.DataFrame
        Output of ``make_bayes_choice_dictionary()``.  Must have columns
        ``"observed"``, ``"standard"``, and ``"p_standard"``.
    lambda_ : float
        Posterior mean of lambda from ``draw_gibbs()``; i.e.
        ``float(np.mean(post["lambda_"]))``.
    delta : float
        Posterior mean of delta from ``draw_gibbs()``; i.e.
        ``float(np.mean(post["delta"]))``.
    method : str
        Distance metric for out-of-dictionary lookups.  Should match the
        metric used to build *dictionary*.  Default ``"jw"``.
    ncores : int
        Number of parallel worker processes for out-of-dictionary names.

    Returns
    -------
    list of str or None
        Standardized names (same length as *names*).
    """
    names_arr = list(names)

    # Warn if overlap between input names and dictionary is small
    dict_observed = set(dictionary["observed"])
    in_dict = [n in dict_observed for n in names_arr]
    overlap = np.mean(in_dict)
    if overlap < 0.5:
        warnings.warn(
            "Overlap between `names` and `dictionary['observed']` is small. "
            "Are you sure you have the right dictionary?",
            stacklevel=2,
        )

    # Build a table of unique standard names (for out-of-dictionary lookups)
    standard_names_df = (
        dictionary.drop_duplicates(subset="standard")
        .drop(columns="observed")
        .reset_index(drop=True)
    )

    # Split off names missing from dictionary
    missing_observed = list(set(n for n in names_arr if n not in dict_observed))

    if missing_observed:
        if ncores > 1:
            pool = multiprocessing.Pool(
                processes=ncores,
                initializer=_init_std_worker,
                initargs=(standard_names_df, lambda_, delta, method),
            )
            missing_standard = list(
                tqdm(
                    pool.imap(_std_worker, missing_observed),
                    total=len(missing_observed),
                    desc="standardize_names (missing)",
                )
            )
            pool.close()
            pool.join()
        else:
            _init_std_worker(standard_names_df, lambda_, delta, method)
            missing_standard = [
                _std_worker(n)
                for n in tqdm(missing_observed, desc="standardize_names (missing)")
            ]

        new_dict = pd.DataFrame({"observed": missing_observed, "standard": missing_standard})
        full_dict = pd.concat(
            [dictionary[["observed", "standard"]], new_dict],
            ignore_index=True,
        )
    else:
        full_dict = dictionary[["observed", "standard"]].copy()

    # Lookup
    lookup = dict(zip(full_dict["observed"], full_dict["standard"]))
    return [lookup.get(n) for n in names_arr]
