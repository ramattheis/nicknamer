"""Session-level cache for expensive downloads (mirrors R's .nicknamerenv)."""

_us_surnames_dictionary_cache = None


def get_us_dictionary_cache():
    return _us_surnames_dictionary_cache


def set_us_dictionary_cache(df):
    global _us_surnames_dictionary_cache
    _us_surnames_dictionary_cache = df
