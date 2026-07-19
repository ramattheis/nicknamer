"""Download and cache the pre-built US surname dictionary."""

import os
import shutil
import sys
import tempfile
from typing import Optional

import pandas as pd

from . import _cache


_US_DICTIONARY_URL = (
    "https://github.com/ramattheis/nicknamer/releases/download/v1.0.0/us_dictionary.rds"
)


def _default_cache_dir() -> str:
    """Return a per-user cache directory (mirrors R's tools::R_user_dir).

    Honors ``NICKNAMER_CACHE_DIR`` if set, otherwise falls back to the
    platform-conventional user cache location using only the standard library.
    """
    override = os.environ.get("NICKNAMER_CACHE_DIR")
    if override:
        return override

    if sys.platform == "darwin":
        base = os.path.expanduser("~/Library/Caches")
    elif sys.platform.startswith("win"):
        base = os.environ.get("LOCALAPPDATA") or os.path.expanduser("~")
    else:
        base = os.environ.get("XDG_CACHE_HOME") or os.path.expanduser("~/.cache")

    return os.path.join(base, "nicknamer")


def load_us_dictionary(
    path: Optional[str] = None,
    cache_dir: Optional[str] = None,
    refresh: bool = False,
    tries: int = 3,
) -> pd.DataFrame:
    """Download, cache, and return the US historical census surname dictionary.

    The dictionary is a large (~104 MB) ``.rds`` file served as a GitHub
    release asset. It is cached both in memory (for the current session) and on
    disk (across sessions) to avoid repeated downloads. If the automatic
    download is unreliable (e.g. on an HPC node with a slow or firewalled
    connection), download the file manually and pass its location via ``path``.

    Parameters
    ----------
    path : str, optional
        Path to a locally downloaded ``us_dictionary.rds`` file. If supplied,
        the file is read directly and no download is attempted.
    cache_dir : str, optional
        Directory used for the persistent on-disk cache. Defaults to a per-user
        cache directory (overridable via the ``NICKNAMER_CACHE_DIR`` environment
        variable). The downloaded dictionary is stored here so subsequent
        sessions load it from disk instead of re-downloading.
    refresh : bool, default False
        If ``True``, ignore any cached copy (in memory or on disk) and download
        a fresh copy.
    tries : int, default 3
        Number of times to attempt the download before giving up.

    Returns
    -------
    pandas.DataFrame
        Data frame with columns ``"observed"`` and ``"standard"``.

    Raises
    ------
    RuntimeError
        If the download or parsing fails.
    """
    import pyreadr

    def _read_rds(file_path: str) -> pd.DataFrame:
        result = pyreadr.read_r(file_path)
        # pyreadr returns an OrderedDict; the data frame is the first value
        return pd.DataFrame(next(iter(result.values())))

    # 1. If the user supplied a local file, use it directly.
    if path is not None:
        if not os.path.exists(path):
            raise FileNotFoundError(f"`path` was supplied but no file exists at: {path}")
        us_dictionary = _read_rds(path)
        _cache.set_us_dictionary_cache(us_dictionary)
        return us_dictionary

    # 2. In-memory cache for the current session.
    if not refresh:
        cached = _cache.get_us_dictionary_cache()
        if cached is not None:
            return cached

    # 3. Persistent on-disk cache across sessions.
    if cache_dir is None:
        cache_dir = _default_cache_dir()
    cache_file = os.path.join(cache_dir, "us_dictionary.rds")
    if not refresh and os.path.exists(cache_file):
        us_dictionary = _read_rds(cache_file)
        _cache.set_us_dictionary_cache(us_dictionary)
        return us_dictionary

    # 4. Download to the persistent cache, with retries.
    os.makedirs(cache_dir, exist_ok=True)

    print("Downloading US surname dictionary (~104 MB)… this might take a while.")

    import requests

    last_error: Optional[Exception] = None
    for attempt in range(1, tries + 1):
        tmp_path: Optional[str] = None
        try:
            with tempfile.NamedTemporaryFile(suffix=".rds", delete=False) as fh:
                tmp_path = fh.name

            # Download to a temp file first, then move into place, so an
            # interrupted download never leaves a truncated cache file.
            # Allow up to 30 minutes for the large file.
            with requests.get(_US_DICTIONARY_URL, stream=True, timeout=1800) as resp:
                resp.raise_for_status()
                with open(tmp_path, "wb") as out:
                    for chunk in resp.iter_content(chunk_size=1 << 20):
                        out.write(chunk)

            us_dictionary = _read_rds(tmp_path)
            shutil.move(tmp_path, cache_file)
            tmp_path = None  # moved; nothing to clean up

            _cache.set_us_dictionary_cache(us_dictionary)
            return us_dictionary

        except Exception as exc:  # noqa: BLE001 - retry on any download/parse error
            last_error = exc
            if attempt < tries:
                print(f"Download attempt {attempt} of {tries} failed; retrying…")
        finally:
            if tmp_path and os.path.exists(tmp_path):
                os.remove(tmp_path)

    raise RuntimeError(
        f"Failed to download the US surname dictionary after {tries} attempts: "
        f"{last_error}\n"
        "You can download it manually and pass it to "
        "load_us_dictionary(path=...):\n"
        f"  {_US_DICTIONARY_URL}"
    )
