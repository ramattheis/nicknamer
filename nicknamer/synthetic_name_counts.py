"""Generate a synthetic dataset of garbled name counts (for testing/demos)."""

import math
import random
import string
from collections import Counter
from typing import List

import pandas as pd


# US state names (equivalent to R's built-in `state.name`)
_STATE_NAMES: List[str] = [
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
    "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
    "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
    "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
    "New Hampshire", "New Jersey", "New Mexico", "New York",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
    "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
    "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
    "West Virginia", "Wisconsin", "Wyoming",
]


def _garble(x: str) -> str:
    """Apply 1–3 random character substitutions (or deletions) to *x*."""
    s = list(x)
    L = len(s)
    n = random.choices([1, 2, 3], weights=[0.6, 0.3, 0.1])[0]
    n = min(n, L)
    positions = random.sample(range(L), n)
    # Pool: all lower-case letters + 30 empty strings (deletions)
    pool = list(string.ascii_lowercase) + [""] * 30
    for pos in positions:
        s[pos] = random.choice(pool)
    return "".join(s)


def synthetic_name_counts() -> pd.DataFrame:
    """Generate a fake dataset of name counts with random garbled variants.

    Starts from the 50 US state names, replicates each name (count depends on
    rank), applies random character substitutions ("garbling"), then tallies
    both clean and garbled occurrences.

    Returns
    -------
    pandas.DataFrame with columns:
        ``name``  – the (possibly garbled) name.
        ``count`` – the number of times it appears.
    """
    names = _STATE_NAMES

    # Build clean list: name[i] repeated floor(2000 / (2*(i+1))) times
    clean_list: List[str] = []
    for i, name in enumerate(names):
        reps = math.floor(2000 / (2 * (i + 1)))
        clean_list.extend([name] * reps)

    # Garble each clean name once
    garbled_list = [_garble(n) for n in clean_list]

    # Combine: 10× clean + 1× garbled
    full_list = clean_list * 10 + garbled_list

    counts = Counter(full_list)
    df = (
        pd.DataFrame(counts.items(), columns=["name", "count"])
        .sort_values("count", ascending=False)
        .reset_index(drop=True)
    )
    return df
