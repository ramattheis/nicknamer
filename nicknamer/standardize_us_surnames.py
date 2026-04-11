"""Standardize surnames using the pre-built US census dictionary."""

import re
from typing import List, Optional

from .load_us_dictionary import load_us_dictionary


def standardize_us_surnames(names: List[str]) -> List[Optional[str]]:
    """Return Bayes-optimal standard surnames for US historical census data.

    Looks up each name in the pre-built US census surname dictionary.
    Names must already be cleaned (e.g. via ``clean_surnames()``).

    Parameters
    ----------
    names : list of str
        Cleaned name strings (lowercase, no digits, ``"?"`` for missing chars).

    Returns
    -------
    list of str or None
        Standardized names (same length as input).  ``None`` for names not
        found in the dictionary.

    Raises
    ------
    ValueError
        If *names* contains upper-case letters, digits, or punctuation other
        than ``"?"``.
    """
    # Guard: reject uncleaned input (mirrors the R check)
    bad = [
        n for n in names
        if re.search(r"[A-Z0-9]", n) or (re.search(r"[[:punct:]]", n, re.ASCII) and "?" not in n)
    ]
    # Simpler portable version of the R check
    def _looks_uncleaned(s: str) -> bool:
        if not s:
            return False
        has_upper_or_digit = bool(re.search(r"[A-Z0-9]", s))
        has_punct_not_q = bool(re.search(r"[^\w?]", s)) or (
            re.search(r"[_]", s) is not None
        )
        return has_upper_or_digit

    if any(_looks_uncleaned(n) for n in names):
        raise ValueError(
            "`names` has upper-case letters or digits. "
            "Apply `clean_surnames()` before `standardize_us_surnames()`."
        )

    us_dictionary = load_us_dictionary()
    lookup = dict(zip(us_dictionary["observed"], us_dictionary["standard"]))
    return [lookup.get(n) for n in names]
