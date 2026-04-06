# nicknamer

`nicknamer` is an R package — with a full Python port — that collects tools for standardizing and cleaning lists of names. It is designed primarily for historical US census surnames but applies broadly to any English-language name data.

---

## How it works

The package implements a **Bayesian garbling model**. The core idea is that an observed name $s_i$ is a noisy version of a true name $s_j$, where the noise process is characterised by two parameters:

- **δ (delta)** — the probability that a name is garbled at all.
- **λ (lambda)** — the exponential decay rate controlling how much garbling is penalised for larger edit distances.

Given a corpus of name counts, the package:

1. Cleans raw strings (`clean_surnames`).
2. Builds a sparse neighbor graph of similar names (`find_neighbors`).
3. Estimates δ and λ from the data via a Gibbs / Metropolis-Hastings sampler (`draw_gibbs`).
4. Constructs a Bayes-optimal mapping from every observed name to its most likely true name (`make_bayes_choice_dictionary`, `standardize_names`).

---

## R installation

```r
# install directly from GitHub
remotes::install_github("ramattheis/nicknamer")
```

---

## Python installation

The Python port mirrors every R function exactly (verified against R output to floating-point precision).

### From GitHub

```bash
pip install "git+https://github.com/ramattheis/nicknamer.git"
```

### From a local clone

```bash
git clone https://github.com/ramattheis/nicknamer.git
cd nicknamer
pip install -e .
```

### Dependencies

`numpy`, `scipy`, `pandas`, `rapidfuzz`, `tqdm`, `requests`, `pyreadr`

These are installed automatically by pip. Python ≥ 3.9 is required.

---

## Python quick-start

```python
from nicknamer import (
    clean_surnames,
    find_neighbors,
    draw_gibbs,
    make_bayes_choice_dictionary,
    standardize_names,
)
import numpy as np

# 1) Clean raw surname strings
raw   = ["SMITH", "O'Brien", "Smyth", "jones jr.", "Smit"]
clean = clean_surnames(raw)
# → ['smith', 'obrien', 'smyth', 'jones', 'smit']

# 2) Build a name-count table (unique names + their frequencies)
names  = ["smith", "smyth", "smit", "jones", "john"]
counts = np.array([500, 40, 15, 300, 80], dtype=float)

# 3) Compute pairwise Jaro-distance neighbor graph
nm = find_neighbors(names, method="jw", max_dist=0.2)
D, M = nm["D"], nm["M"]

# 4) Estimate δ and λ via Gibbs sampling
post = draw_gibbs(D, M, n_obs=counts, n_iter=5000)
delta  = float(np.mean(post["delta"][2500:]))
lambda_ = float(np.mean(post["lambda_"][2500:]))

# 5) Build Bayes-optimal dictionary
p = counts / counts.sum()
dictionary = make_bayes_choice_dictionary(names, D, p, delta, lambda_)

# 6) Standardize a new list of observed names
observed = ["smyth", "smit", "jones", "smith"]
standard = standardize_names(observed, dictionary, lambda_=lambda_, delta=delta)
```

### US historical census shortcut

If you only need to standardize US historical census surnames, a pre-built dictionary is available:

```python
from nicknamer import clean_surnames, standardize_us_surnames

raw      = ["SMITH", "Smythe", "O'Brien", "Joans"]
cleaned  = clean_surnames(raw)
standard = standardize_us_surnames(cleaned)
```

The first call downloads the dictionary (~30 MB) and caches it for the rest of the session.

---

## Running the tests

The test suite validates every Python function against reference output generated from the R package:

```bash
# Generate fresh R reference data (requires R + nicknamer installed)
Rscript tests/generate_r_reference.R

# Run comparisons
python3 tests/test_vs_r.py
```

---

## Function reference

| Python function | R equivalent | Description |
|---|---|---|
| `clean_surnames(raw)` | `clean_surnames()` | Normalize raw surname strings |
| `find_neighbors(names, method, max_dist)` | `find_neighbors()` | Sparse Jaro / Levenshtein neighbor matrices |
| `make_kernel(D, M, delta, lambda_)` | `make_kernel()` | K×K garbling transition kernel |
| `loglikelihood(p, D, M, delta, lambda_, n_obs)` | `loglikelihood()` | Log-likelihood under the garbling model |
| `rdirichlet(alpha)` | `rdirichlet()` | Single Dirichlet draw |
| `draw_gibbs(D, M, n_obs, ...)` | `draw_gibbs()` | Gibbs + MH posterior sampler |
| `make_bayes_choice_dictionary(names, D, p, delta, lambda_)` | `make_bayes_choice_dictionary()` | Bayes-optimal observed→standard map |
| `standardize_missing_name(name, std_df, lambda_, delta, method)` | `standardize_missing_name()` | Single out-of-dictionary lookup |
| `standardize_names(names, dictionary, lambda_, delta)` | `standardize_names()` | Vectorised standardisation |
| `load_us_dictionary()` | `load_us_dictionary()` | Download & cache US census dictionary |
| `standardize_us_surnames(names)` | `standardize_us_surnames()` | US census surname lookup |
| `synthetic_name_counts()` | `synthetic_name_counts()` | Synthetic garbled name-count dataset |

> **Note:** Python uses `lambda_` (trailing underscore) everywhere `lambda` appears in R, since `lambda` is a reserved keyword in Python.
