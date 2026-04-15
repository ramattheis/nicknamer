"""Map first names to their canonical forms via a nickname dictionary."""

from typing import List, Optional, Union

import pandas as pd


def standardize_nicknames(
    names: List[Optional[str]],
    sex: Union[None, str, List[Optional[str]]] = None,
    dictionary: Optional[pd.DataFrame] = None,
) -> List[Optional[str]]:
    """Map first names to their canonical forms via a nickname dictionary.

    An **optional post-processing step** that maps already
    spelling-standardized first names to their canonical forms using a
    nickname dictionary.  For example, ``"bob"`` → ``"robert"``,
    ``"betty"`` → ``"elizabeth"``.

    This function is intended to be applied *after* Bayesian spelling
    standardization (via :func:`draw_gibbs` / :func:`make_bayes_choice_dictionary`),
    not as a substitute for it.

    When ``sex`` is supplied, only canonical forms of the matching sex are
    considered for ambiguous nicknames (e.g. ``"alex"`` → ``"alexander"`` for
    males, ``"alexandra"`` for females).  If no sex-specific match exists, the
    function falls back to all available matches.

    Names that do not appear in the dictionary are returned unchanged.

    Parameters
    ----------
    names : list of str or None
        Cleaned, spelling-standardized first names.
    sex : None, str, or list of str / None
        ``None`` (default) — no sex filtering.
        A single string (``"male"`` or ``"female"``) — applied to all names.
        A list the same length as ``names`` — per-record sex filter.
        Individual ``None`` values skip sex filtering for that record.
    dictionary : pandas.DataFrame or None
        Data frame with columns ``nickname``, ``canonical``, ``sex``
        (output of :func:`load_nickname_dictionary`).  If ``None``, the
        dictionary is downloaded / returned from cache automatically.

    Returns
    -------
    list of str or None
        Same length as ``names``.
    """
    if dictionary is None:
        from .load_nickname_dictionary import load_nickname_dictionary
        dictionary = load_nickname_dictionary()

    # Validate / expand sex argument
    sex_vec: Optional[List[Optional[str]]] = None
    if sex is not None:
        if isinstance(sex, str):
            sex_vec = [sex.lower()] * len(names)
        else:
            sex_vec = [s.lower() if s is not None else None for s in sex]
            if len(sex_vec) != len(names):
                raise ValueError(
                    "`sex` must be None, a single string, or the same "
                    "length as `names`."
                )
        for s in sex_vec:
            if s is not None and s not in ("male", "female"):
                raise ValueError('`sex` values must be "male", "female", or None.')

    # Build a lookup: nickname → list of (canonical, sex) rows
    lookup: dict = {}
    for _, row in dictionary.iterrows():
        nick = row["nickname"]
        if pd.isna(nick):
            continue
        lookup.setdefault(nick, []).append((row["canonical"], row["sex"]))

    result: List[Optional[str]] = list(names)

    for i, nm in enumerate(names):
        if nm is None or nm == "":
            continue

        candidates = lookup.get(nm)
        if not candidates:
            continue   # Not a known nickname — leave unchanged

        # Apply sex filter if requested
        if sex_vec is not None and sex_vec[i] is not None:
            sex_filtered = [
                (c, sx) for c, sx in candidates
                if sx == sex_vec[i]
            ]
            if sex_filtered:
                candidates = sex_filtered
            # If no sex-specific match, fall back to all candidates

        result[i] = candidates[0][0]

    return result
