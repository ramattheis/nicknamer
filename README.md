# nicknamer

<<<<<<< HEAD
`nicknamer` is an R package for standardizing and cleaning lists of names. It was designed primarily for English-language surnames in historical US census data, where names are frequently misspelled, truncated, or otherwise garbled. The package provides both a fast-path wrapper for US surnames and a general-purpose pipeline for any name list, grounded in an simple probabilistic model of name garbling. This version of `nicknamer` was made with the help of Claude Opus 4.6. 

## Installation

```r
# install.packages("devtools")
devtools::install_github("ramattheis/nicknamer")
```

## Workflow

### US historical surnames 

For US historical surnames specifically, `standardize_us_surnames()` bundles a pre-fitted dictionary and skips steps 2–4 entirely:

```r
standard_names <- standardize_us_surnames(clean_surnames(raw_names))
```

### Any list of names

A full standardization run has four steps.

**Step 1 — Clean raw names.** `clean_surnames()` normalizes a raw character vector into a form suitable for string-distance comparisons. It lowercases, removes suffixes (Jr./Sr.), strips spaces and apostrophes, and blanks out entries that are too short, too ambiguous, or contain too many missing-character markers (recorded as `?`).

```r
dt[, name := clean_surnames(namelast)]
```

**Step 2 — Build the neighbor graph.** `find_neighbors()` computes pairwise string distances between all unique names and returns two sparse matrices: `D` (distances for pairs within `max_dist`) and `M` (a binary mask of which pairs are "neighbors"). Parallelization is supported via `ncores`. The default metric is Jaro-Winkler (`"jw"`), which tends to work well for name data, but any metric supported by `stringdist` can be used.

```r
nmats <- find_neighbors(names = unique_names, method = "jw", max_dist = 0.2, ncores = 10)
D <- nmats$D
M <- nmats$M
```

**Step 3 — Estimate model parameters via Gibbs sampling.** `draw_gibbs()` runs an MH-within-Gibbs sampler to estimate the posterior distribution of the two garbling parameters (δ and λ; see the model description below) and the true name frequency vector p. Standard convergence diagnostics apply: inspect the trace plots of δ and λ, and check the MH acceptance rate.

```r
out <- draw_gibbs(D = D, M = M, n_obs = name_counts, n_iter = 10000)
plot(out$delta,  type = "l", main = "Trace of δ")
plot(out$lambda, type = "l", main = "Trace of λ")
```

**Step 4 — Standardize names.** `make_bayes_choice_dictionary()` constructs a lookup table mapping each observed name to its Bayes-optimal standard form, using the posterior means of δ and λ. `standardize_names()` then applies this dictionary to a new name vector, with optional parallelization.

```r
dictionary <- make_bayes_choice_dictionary(
  names  = unique_names,
  D      = D,
  p      = out$p_mean,
  delta  = mean(out$delta),
  lambda = mean(out$lambda)
)

standard_names <- standardize_names(names = my_names, dictionary = dictionary,
                             lambda = mean(out$lambda), delta = mean(out$delta))
=======
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
