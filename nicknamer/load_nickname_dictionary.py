"""Download and cache the nickname dictionary."""

import csv
import io
from typing import Optional

import pandas as pd
import requests

_NICKNAME_URL = (
    "https://raw.githubusercontent.com/carltonnorthern/"
    "nicknames/master/names.csv"
)

# Session-level cache (mirrors R's .nicknamerenv)
_nickname_dictionary_cache: Optional[pd.DataFrame] = None

# Manual corrections for cases where carltonnorthern's bidirectional data
# produces the wrong mapping.  Each entry overrides whatever the dataset says.
# Format: {nickname: canonical}
_CORRECTIONS = {
    "bill":  "william",
    "billy": "william",
    "billa": "william",
}

# Sex lookup for common English given names.
# Used to tag canonical forms so ambiguous nicknames (e.g. "alex") can be
# resolved when a sex filter is provided.
_MALE_NAMES = {
    "aaron", "abraham", "adam", "albert", "alexander", "alfred", "andrew",
    "archibald", "augustus", "barnabas", "bartholomew", "benjamin", "caleb",
    "carl", "charles", "christian", "christopher", "clement", "cornelius",
    "cyrus", "daniel", "david", "dennis", "donald", "douglas", "edgar",
    "edward", "elijah", "elisha", "ernest", "eugene", "ezekiel", "ezra",
    "felix", "ferdinand", "francis", "frank", "franklin", "frederick",
    "george", "gerald", "gordon", "harold", "harry", "henry", "herbert",
    "herman", "hiram", "homer", "horace", "howard", "humphrey", "isaac",
    "isaiah", "jacob", "james", "jasper", "jeremiah", "jerome", "joel",
    "john", "jonathan", "joseph", "josiah", "julius", "kenneth", "lambert",
    "lawrence", "leonard", "lewis", "louis", "marcus", "martin", "matthew",
    "michael", "miles", "morgan", "moses", "nathaniel", "nicholas", "noah",
    "norbert", "norman", "oliver", "oscar", "patrick", "paul", "peter",
    "philip", "ralph", "raymond", "reginald", "richard", "robert", "roger",
    "rudolph", "samuel", "sebastian", "simon", "solomon", "stephen",
    "sylvester", "theodore", "thomas", "timothy", "tobias", "victor",
    "vincent", "walter", "warren", "wendell", "william", "zachary",
}

_FEMALE_NAMES = {
    "abigail", "adelaide", "agatha", "agnes", "alice", "alexandra",
    "amanda", "amelia", "anastasia", "ann", "anna", "anne", "arabella",
    "barbara", "beatrice", "bertha", "caroline", "carolyn", "catharine",
    "catherine", "cecilia", "charlotte", "clara", "clemence", "constance",
    "cordelia", "dorothy", "edna", "eleanor", "eliza", "elizabeth", "ella",
    "ellen", "eloise", "emily", "emma", "ernestine", "esther", "ethel",
    "eugenia", "eva", "evelyn", "fannie", "fanny", "felicia", "flora",
    "florence", "frances", "gertrude", "grace", "harriet", "helen",
    "henrietta", "hester", "ida", "irene", "isabel", "isabella", "jane",
    "janet", "jean", "jennie", "jenny", "jessie", "joan", "josephine",
    "judith", "julia", "juliet", "kate", "katharine", "katherine", "laura",
    "lavinia", "leonora", "louisa", "louise", "lucia", "lucy", "lydia",
    "mabel", "margaret", "maria", "marian", "marianne", "martha", "mary",
    "matilda", "maud", "maude", "millicent", "minnie", "miriam", "nancy",
    "nellie", "olive", "patricia", "pauline", "pearl", "penelope", "phyllis",
    "priscilla", "rachel", "rebecca", "rhoda", "rosa", "rosalie", "rosanna",
    "rose", "ruth", "sallie", "samantha", "sarah", "selina", "sophia",
    "sue", "susan", "susanna", "susannah", "sylvia", "theresa", "virginia",
    "wilhelmina", "winifred",
}


def _sex_of(canonical: str) -> Optional[str]:
    if canonical in _MALE_NAMES:
        return "male"
    if canonical in _FEMALE_NAMES:
        return "female"
    return None


def load_nickname_dictionary() -> pd.DataFrame:
    """Download and cache the nickname dictionary.

    Downloads the nickname list maintained by Carlton Northern
    (https://github.com/carltonnorthern/nicknames) and returns it as a
    tidy DataFrame mapping each nickname to its canonical form.  The
    dictionary is cached for the Python session so subsequent calls are
    instantaneous.

    The raw CSV format is ``canonical,has_nickname,nickname``.  This
    function inverts the relationship so each row is a
    *nickname → canonical* mapping.  A ``sex`` column is appended using
    a hardcoded lookup of common English given names; entries whose
    canonical form is not in the lookup receive ``None`` for sex.

    Returns
    -------
    pandas.DataFrame
        Columns: ``nickname`` (str), ``canonical`` (str), ``sex``
        (``"male"``, ``"female"``, or ``None``).
    """
    global _nickname_dictionary_cache
    if _nickname_dictionary_cache is not None:
        return _nickname_dictionary_cache

    print("Downloading nickname dictionary...")

    resp = requests.get(_NICKNAME_URL, timeout=60)
    resp.raise_for_status()

    reader = csv.reader(io.StringIO(resp.text))
    rows = []
    for row in reader:
        if len(row) < 3:
            continue
        canonical, relationship, nickname = row[0], row[1], row[2]
        if relationship.strip() != "has_nickname":
            continue
        canonical = canonical.strip().lower()
        nickname  = nickname.strip().lower()
        if canonical and nickname:
            rows.append({
                "nickname":  nickname,
                "canonical": canonical,
                "sex":       _sex_of(canonical),
            })

    df = pd.DataFrame(rows, columns=["nickname", "canonical", "sex"])

    # The carltonnorthern dataset is bidirectional — it contains both
    # bill→william and william→bill.  We only keep mappings whose canonical
    # is a recognised full given name (in our sex lookup), so that informal
    # forms always point *to* formal ones, never the other way around.
    # e.g. bill→william ✓, william→bill ✗; bob→robert ✓, robert→bob ✗.
    known_full_names = _MALE_NAMES | _FEMALE_NAMES
    df = df[df["canonical"].isin(known_full_names)].reset_index(drop=True)

    # When a nickname maps to multiple canonicals of the same sex, prefer the
    # one with the most nickname entries in the dataset (the most "prominent"
    # full name).  This resolves e.g. bill→william over bill→robert, because
    # william has far more nicknames pointing to it than robert does.
    canonical_prominence = df.groupby("canonical").size().rename("prominence")
    df = df.join(canonical_prominence, on="canonical")
    df = (df.sort_values("prominence", ascending=False)
            .drop_duplicates(subset=["nickname", "sex"], keep="first")
            .drop(columns="prominence")
            .reset_index(drop=True))

    # Apply manual corrections
    for nick, canon in _CORRECTIONS.items():
        mask = df["nickname"] == nick
        if mask.any() and canon in known_full_names:
            df.loc[mask, "canonical"] = canon
            df.loc[mask, "sex"] = _sex_of(canon)

    _nickname_dictionary_cache = df
    return df
