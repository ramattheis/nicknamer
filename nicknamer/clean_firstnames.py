"""Clean a vector of (English-language) first names."""

import re
import unicodedata
from typing import List, Optional


# Conservative census abbreviation expansion table.
# Only whole-name, unambiguous entries are included.
# "J" is deliberately excluded (could be John, James, Joseph, …).
_ABBR_TABLE: dict = {
    "wm":      "william",
    "thos":    "thomas",
    "jno":     "john",
    "jhn":     "john",
    "jas":     "james",
    "chas":    "charles",
    "geo":     "george",
    "benj":    "benjamin",
    "robt":    "robert",
    "richd":   "richard",
    "edwd":    "edward",
    "saml":    "samuel",
    "danl":    "daniel",
    "michl":   "michael",
    "nathl":   "nathaniel",
    "alexr":   "alexander",
    "abm":     "abraham",
    "christr": "christopher",
    "nichs":   "nicholas",
    "andr":    "andrew",
    "lawr":    "lawrence",
    "archd":   "archibald",
    "eliz":    "elizabeth",
}

# Pre-compiled regex patterns for missing-letter marker normalisation
# (mirrors clean_surnames step 5 / R PCRE patterns).
_STEP5_PATTERNS = [
    re.compile(r"(?<=[a-z]{2})[^a-z](?=[a-z])"),
    re.compile(r"(?<=[a-z])[^a-z](?=[a-z]{2})"),
    re.compile(r"(?<=[a-z]{3})[^a-z]{2}(?=[a-z])"),
    re.compile(r"(?<=[a-z])[^a-z]{2}(?=[a-z]{3})"),
    re.compile(r"(^[^a-z]{1,2}(?=[a-z]{4}))|((?<=[a-z]{4})[^a-z]{1,2}$)"),
]


def _to_ascii(s: str) -> str:
    """Transliterate accented / non-ASCII characters to their ASCII base."""
    return unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode("ascii")


def clean_firstnames(
    raw_names: List[Optional[str]],
    expand_abbreviations: bool = True,
    drop_middle: bool = True,
) -> List[Optional[str]]:
    """Clean a list of (English-language) first names.

    Takes a list of raw first-name strings and returns cleaned strings
    suitable for string-distance comparisons.  Rules follow the approach of
    Abramitzky & Boustan / Yildirim for historical US census first names.

    Unlike ``clean_surnames()``, **single-character names are not blanked**,
    because abbreviated first names such as "J" or "E" are common and
    informative in historical census records.

    Parameters
    ----------
    raw_names : list of str or None
    expand_abbreviations : bool
        If ``True`` (default), replace known census abbreviations with their
        full forms (e.g. ``"wm"`` → ``"william"``).
    drop_middle : bool
        If ``True`` (default), drop any middle name or initial, keeping only
        the first whitespace-delimited token.

    Returns
    -------
    list of str or None
        Cleaned first names (same length as input).  ``None`` inputs return
        ``None``.

    Notes
    -----
    Abbreviation expansion table (23 conservative entries):

    =========  ===========
    Abbrev.    Expansion
    =========  ===========
    wm         william
    thos       thomas
    jno        john
    jhn        john
    jas        james
    chas       charles
    geo        george
    benj       benjamin
    robt       robert
    richd      richard
    edwd       edward
    saml       samuel
    danl       daniel
    michl      michael
    nathl      nathaniel
    alexr      alexander
    abm        abraham
    christr    christopher
    nichs      nicholas
    andr       andrew
    lawr       lawrence
    archd      archibald
    eliz       elizabeth
    =========  ===========
    """
    # Track None positions — return None for None input (mirrors R NA→NA)
    none_mask = [s is None for s in raw_names]
    cleaned = ["" if s is None else str(s) for s in raw_names]

    # 1) Unicode → ASCII
    cleaned = [_to_ascii(s) for s in cleaned]

    # 2) Lowercase
    cleaned = [s.lower() for s in cleaned]

    # 3) Remove common trailing suffixes (jr, sr, i, ii, iii, iv)
    cleaned = [re.sub(r"\s+(jr\.?|sr\.?|i{1,3}|iv)\s*$", "", s) for s in cleaned]

    # 4) Strip leading/trailing whitespace
    cleaned = [s.strip() for s in cleaned]

    # 5) Optionally drop middle name/initial (keep first token only)
    if drop_middle:
        cleaned = [s.split()[0] if s.split() else s for s in cleaned]

    # 6) Strip apostrophes
    cleaned = [s.replace("'", "") for s in cleaned]

    # 7) Remove digits
    cleaned = [re.sub(r"[0-9]", "", s) for s in cleaned]

    # 8) Standardise "missing-letter" markers
    for pat in _STEP5_PATTERNS:
        cleaned = [pat.sub(" ", s) for s in cleaned]

    # 9) Blank out names with remaining non-letter / non-space chars
    cleaned = [s if not re.search(r"[^a-z ]", s) else "" for s in cleaned]

    # 10) Replace spaces with ?
    cleaned = [s.replace(" ", "?") for s in cleaned]

    # 11) Blank out known placeholder strings
    _placeholders = ("unreadable", "unknown", "?blank?", "alias")
    cleaned = ["" if any(p in s for p in _placeholders) else s for s in cleaned]

    # 12) Blank out names with too many missing characters (> 25 %)
    #     Unlike clean_surnames, single-char names are NOT blanked.
    def _too_many_missing(s: str) -> bool:
        if not s:
            return False
        n_letters = len(s.replace("?", ""))
        return (n_letters / len(s)) < 0.75

    cleaned = ["" if _too_many_missing(s) else s for s in cleaned]

    # 13) Expand abbreviations (whole-name matches only)
    if expand_abbreviations:
        cleaned = [_ABBR_TABLE.get(s, s) for s in cleaned]

    # Restore None positions
    cleaned = [None if none_mask[i] else cleaned[i] for i in range(len(cleaned))]

    return cleaned
