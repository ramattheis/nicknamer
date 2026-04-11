# nicknamer

`nicknamer` is an R package — with a full Python port — for standardizing and cleaning lists of names. It was designed primarily for English-language surnames in historical US census data, where names are frequently misspelled, truncated, or otherwise garbled. The package provides both a fast-path wrapper for US surnames and a general-purpose pipeline for any name list, grounded in a simple probabilistic model of name garbling. This version of `nicknamer` was made with the help of Claude Opus 4.6.

---

## R installation

```r
# install.packages("devtools")
devtools::install_github("ramattheis/nicknamer")
```

## Python installation

The Python port mirrors every R function exactly, verified against R output to floating-point precision.

```bash
pip install "git+https://github.com/ramattheis/nicknamer.git"
```

Or from a local clone:

```bash
git clone https://github.com/ramattheis/nicknamer.git
cd nicknamer
pip install -e .
```

Requires Python ≥ 3.9. Dependencies (`numpy`, `scipy`, `pandas`, `rapidfuzz`, `tqdm`, `requests`, `pyreadr`) are installed automatically.

---

## Workflow

### US historical surnames

For US historical surnames specifically, `standardize_us_surnames()` bundles a pre-fitted dictionary and skips steps 2–4 entirely:

```r
standard_names <- standardize_us_surnames(clean_surnames(raw_names))
```

```python
from nicknamer import clean_surnames, standardize_us_surnames
standard_names = standardize_us_surnames(clean_surnames(raw_names))
```

The first call downloads the dictionary (~30 MB) and caches it for the rest of the session.

### Any list of names

A full standardization run has four steps.

**Step 1 — Clean raw names.** `clean_surnames()` normalizes a raw character vector into a form suitable for string-distance comparisons. It lowercases, removes suffixes (Jr./Sr.), strips spaces and apostrophes, and blanks out entries that are too short, too ambiguous, or contain too many missing-character markers (recorded as `?`).

```r
dt[, name := clean_surnames(namelast)]
```

**Step 2 — Build the neighbor graph.** `find_neighbors()` computes pairwise string distances between all unique names and returns two sparse matrices: `D` (distances for pairs within `max_dist`) and `M` (a binary mask of which pairs are "neighbors"). Parallelization is supported via `ncores`. The default metric is Jaro (`"jw"`), which tends to work well for name data, but any metric supported by `stringdist` can be used.

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
```

For Python, the same four steps use identical logic (note `lambda_` instead of `lambda`, which is a reserved keyword):

```python
from nicknamer import (
    clean_surnames, find_neighbors, draw_gibbs,
    make_bayes_choice_dictionary, standardize_names,
)
import numpy as np

names  = clean_surnames(raw_names)
nm     = find_neighbors(names, method="jw", max_dist=0.2)
post   = draw_gibbs(nm["D"], nm["M"], n_obs=counts, n_iter=10000)

delta   = float(np.mean(post["delta"][5000:]))
lambda_ = float(np.mean(post["lambda_"][5000:]))

dictionary     = make_bayes_choice_dictionary(names, nm["D"], post["p_mean"], delta, lambda_)
standard_names = standardize_names(observed_names, dictionary, lambda_=lambda_, delta=delta)
```

---

## The garbling model

The probabilistic model formalizes the idea that what an enumerator writes down is either the true surname, or a garbled variant drawn from the set of nearby names. There are three latent quantities:

- **p** — a K-vector of true population name frequencies, where K is the number of distinct names.
- **δ ∈ (0, 1)** — the *garbling probability*: the chance that a recorded name has been corrupted.
- **λ > 0** — the *decay rate*: controls how steeply the probability of corruption falls off with string distance.

### Observation model

For each person in the data, their true surname is drawn from the population distribution p. With probability (1 − δ) it is recorded correctly; with probability δ it is "mutated" to a neighboring name j, with probability proportional to exp(−λ · d_{ij}), where d_{ij} is the string distance between names i and j (restricted to the neighbor graph M).

This defines a K × K transition kernel K(i | j) = P(observed = i | true = j):

- **On the diagonal** (i = j): K(i | i) = 1 − δ
- **Off the diagonal** (i ≠ j, neighbors): K(i | j) = δ · E(j, i), where E is the row-normalized weight matrix with E(j, i) ∝ exp(−λ · d_{ji}) · M_{ji}
- **Non-neighbors**: K(i | j) = 0

The marginal probability of observing name i is then the mixture L_i = Σ_j p_j · K(i | j), and the log-likelihood of the observed name counts n_obs is Σ_i n_obs,i · log(L_i).

### Priors

- **p** ~ Dirichlet(α), with α = 1 (uniform) by default.
- **δ** ~ Beta(9, 1) by default, encoding a prior belief that most names are recorded correctly.
- **λ** ~ Gamma(1, 0.1) by default.

All priors can be overridden in `draw_gibbs()`.

### Posterior inference

`draw_gibbs()` uses an MH-within-Gibbs sampler:

1. **p** is updated via a collapsed Gibbs step. Expected counts are computed from the current kernel, and a new p is drawn from the resulting Dirichlet posterior.
2. **δ and λ** are updated jointly via a Metropolis-Hastings random walk in logit(δ) / log(λ) space, with a Jacobian correction so that the proposal is symmetric on the transformed scale.

---

## Functions

| Function | Description |
|---|---|
| `clean_surnames()` | Normalize raw surname strings for downstream processing |
| `find_neighbors()` | Build sparse distance (D) and mask (M) matrices |
| `draw_gibbs()` | MH-within-Gibbs sampler for δ, λ, and p |
| `make_kernel()` | Construct the K×K garbling transition kernel |
| `loglikelihood()` | Evaluate the log-likelihood of observed counts |
| `make_bayes_choice_dictionary()` | Map each name to its Bayes-optimal standard form |
| `standardize_names()` | Apply a dictionary to standardize a name vector |
| `standardize_us_surnames()` | Fast-path standardization for US historical surnames |
| `load_us_dictionary()` | Download and cache the US census surname dictionary |
| `synthetic_name_counts()` | Generate synthetic garbled name-count data for testing |
| `rdirichlet()` | Draw a single sample from a Dirichlet distribution |
