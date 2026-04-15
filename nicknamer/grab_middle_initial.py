"""Extract the middle initial from a raw first-name string."""

import re
from typing import List, Optional


def grab_middle_initial(raw_names: List[Optional[str]]) -> List[Optional[str]]:
    """Extract the middle initial from raw first-name strings.

    Inspects each raw first-name string (which may include a middle name or
    initial) and returns the middle initial as a single upper-case character,
    or ``None`` if no middle name / initial is present.

    Common trailing suffixes (Jr, Sr, I, II, III, IV) are stripped before
    splitting so they are not mistaken for middle names.

    Parameters
    ----------
    raw_names : list of str or None

    Returns
    -------
    list of str or None
        Single upper-case letters (middle initials) or ``None`` where no
        middle name is present.  Same length as ``raw_names``.
    """
    none_mask = [s is None for s in raw_names]
    cleaned = ["" if s is None else str(s) for s in raw_names]

    # Lowercase and strip trailing suffixes
    cleaned = [s.lower() for s in cleaned]
    cleaned = [re.sub(r"\s+(jr\.?|sr\.?|i{1,3}|iv)\s*$", "", s) for s in cleaned]
    cleaned = [s.strip() for s in cleaned]

    # Split on whitespace; second token's first letter is the middle initial
    result: List[Optional[str]] = []
    for i, s in enumerate(cleaned):
        if none_mask[i]:
            result.append(None)
            continue
        tokens = s.split()
        if len(tokens) >= 2 and tokens[1]:
            result.append(tokens[1][0].upper())
        else:
            result.append(None)

    return result
