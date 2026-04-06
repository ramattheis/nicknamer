"""Download and cache the pre-built US surname dictionary."""

import os
import tempfile
from typing import Optional

import pandas as pd

from . import _cache


_US_DICTIONARY_URL = (
    "https://github.com/ramattheis/nicknamer/releases/download/v1.0.0/us_dictionary.rds"
)


def load_us_dictionary() -> pd.DataFrame:
    """Download, cache, and return the US historical census surname dictionary.

    The dictionary is downloaded once per Python session and kept in memory.
    It is stored as an ``.rds`` file on GitHub and is read using ``pyreadr``.

    Returns
    -------
    pandas.DataFrame
        Data frame with columns ``"observed"`` and ``"standard"``.

    Raises
    ------
    RuntimeError
        If the download or parsing fails.
    """
    cached = _cache.get_us_dictionary_cache()
    if cached is not None:
        return cached

    print("Downloading US surname dictionary… this might take a while.")

    tmp_path: Optional[str] = None
    try:
        import requests
        import pyreadr

        with tempfile.NamedTemporaryFile(suffix=".rds", delete=False) as fh:
            tmp_path = fh.name

        # Allow up to 30 minutes for a large file
        with requests.get(_US_DICTIONARY_URL, stream=True, timeout=1800) as resp:
            resp.raise_for_status()
            with open(tmp_path, "wb") as fh:
                for chunk in resp.iter_content(chunk_size=1 << 20):
                    fh.write(chunk)

        result = pyreadr.read_r(tmp_path)
        # pyreadr returns an OrderedDict; the data frame is the first value
        us_dictionary = next(iter(result.values()))
        us_dictionary = pd.DataFrame(us_dictionary)

        _cache.set_us_dictionary_cache(us_dictionary)
        return us_dictionary

    except Exception as exc:
        raise RuntimeError(f"Failed to download or load US dictionary: {exc}") from exc
    finally:
        if tmp_path and os.path.exists(tmp_path):
            os.remove(tmp_path)
