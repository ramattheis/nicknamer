"""Clean a vector of (English-language) surnames."""

import re
from typing import List


def clean_surnames(raw_names: List[str]) -> List[str]:
    """Clean a list of (English-language) surnames.

    Takes a list of raw surname strings and returns a list of "cleaned"
    strings (or empty strings where input is too ambiguous).  Rules are
    designed for historical US census data but apply broadly to
    English-language surnames.

    Steps
    -----
    1. Convert to lowercase.
    2. Remove trailing "jr" / "sr" suffixes.
    3. Strip spaces and apostrophes.
    4. Remove all digits.
    5. Replace plausible "missing-letter" markers with ``"?"``.
    6. Blank out names that still contain non-letter/non-space characters.
    7. Replace spaces with ``"?"``.
    8. Blank out known placeholder strings.
    9. Blank out names of length ≤ 1, and uncommon 2-character names.
    10. Blank out names where > 25 % of characters are missing (``"?"``).

    Parameters
    ----------
    raw_names : list of str

    Returns
    -------
    list of str
        Cleaned surnames (same length as input).
    """
    # Track None positions — R returns NA for NA input
    none_mask = [s is None for s in raw_names]
    cleaned = ["" if s is None else s for s in raw_names]

    # 1) lowercase
    cleaned = [s.lower() for s in cleaned]

    # 2) remove trailing jr / sr (with optional period)
    cleaned = [re.sub(r"\s+(jr|sr)\.?$", "", s) for s in cleaned]

    # 3) strip spaces and apostrophes
    cleaned = [re.sub(r"[ ']", "", s) for s in cleaned]

    # 4) remove digits
    cleaned = [re.sub(r"[0-9]", "", s) for s in cleaned]

    # 5) standardise "missing-letter" markers
    # These patterns mirror the R PCRE regexes; Python re supports fixed-length
    # lookbehind/lookahead, which all patterns below use.
    _step5_patterns = [
        (re.compile(r"(?<=[a-z]{2})[^a-z](?=[a-z])"), " "),
        (re.compile(r"(?<=[a-z])[^a-z](?=[a-z]{2})"), " "),
        (re.compile(r"(?<=[a-z]{3})[^a-z]{2}(?=[a-z])"), " "),
        (re.compile(r"(?<=[a-z])[^a-z]{2}(?=[a-z]{3})"), " "),
        (re.compile(r"(^[^a-z]{1,2}(?=[a-z]{4}))|((?<=[a-z]{4})[^a-z]{1,2}$)"), " "),
    ]
    for pat, repl in _step5_patterns:
        cleaned = [pat.sub(repl, s) for s in cleaned]

    # 6) blank out names still containing non-letter / non-space characters
    cleaned = [s if not re.search(r"[^a-z ]", s) else "" for s in cleaned]

    # 7) replace spaces with ?
    cleaned = [s.replace(" ", "?") for s in cleaned]

    # 8) blank out known placeholder strings
    _placeholders = ("unreadable", "unknown", "?blank?", "alias")
    cleaned = ["" if any(p in s for p in _placeholders) else s for s in cleaned]

    # 9) blank out 1-char names; blank out uncommon 2-char names
    _valid_2char = {
        "ah", "ng", "ho", "ma", "ha", "lu", "la", "ba", "on", "wo",
        "an", "le", "un", "lo", "mo", "ca", "bo", "wa", "li",
        "go", "co", "he", "wu", "su", "da", "jo", "yu", "bu", "hi",
        "ko", "me", "ek", "ax", "re", "sy", "ey", "ox",
    }
    result = []
    for s in cleaned:
        if len(s) <= 1:
            result.append("")
        elif len(s) == 2 and s not in _valid_2char:
            result.append("")
        else:
            result.append(s)
    cleaned = result

    # 10) blank out names with too many missing characters (> 25 %)
    def _too_many_missing(s: str) -> bool:
        if not s:
            return False
        n_letters = len(s.replace("?", ""))
        return (n_letters / len(s)) < 0.75

    cleaned = ["" if _too_many_missing(s) else s for s in cleaned]

    # Restore None positions (R returns NA for NA input)
    cleaned = [None if none_mask[i] else cleaned[i] for i in range(len(cleaned))]

    return cleaned
