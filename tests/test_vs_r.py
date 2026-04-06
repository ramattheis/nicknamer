"""
Compare Python nicknamer output against R reference data.

Run with:
    python3 tests/test_vs_r.py
"""

import json
import math
import sys
import os

import numpy as np
import scipy.sparse as sp
import pandas as pd

# Make sure the local package is importable
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from nicknamer.clean_surnames import clean_surnames
from nicknamer.make_kernel import make_kernel
from nicknamer.loglikelihood import loglikelihood
from nicknamer.find_neighbors import find_neighbors
from nicknamer.make_bayes_choice_dictionary import make_bayes_choice_dictionary
from nicknamer.standardize_missing_name import standardize_missing_name
from nicknamer.synthetic_name_counts import synthetic_name_counts

REF = os.path.join(os.path.dirname(__file__), "r_reference")

PASS = "\033[32mPASS\033[0m"
FAIL = "\033[31mFAIL\033[0m"

failures = []


def load(name):
    with open(os.path.join(REF, name)) as f:
        return json.load(f)


def check(test_name, condition, msg=""):
    if condition:
        print(f"  {PASS}  {test_name}")
    else:
        print(f"  {FAIL}  {test_name}" + (f": {msg}" if msg else ""))
        failures.append(test_name)


def allclose(a, b, tol=1e-5):
    a, b = np.asarray(a, float), np.asarray(b, float)
    return bool(np.allclose(a, b, atol=tol, rtol=tol, equal_nan=False))


# =============================================================================
print("\n=== 1. clean_surnames ===")
# =============================================================================
ref = load("clean_surnames.json")
py_out = clean_surnames(ref["input"])

# Element-wise comparison (None ↔ null in JSON)
match = all(
    (py_out[i] is None and ref["output"][i] is None)
    or (py_out[i] == ref["output"][i])
    for i in range(len(ref["output"]))
)
check("all elements match R output", match,
      msg=[(ref["input"][i], ref["output"][i], py_out[i])
           for i in range(len(ref["output"])) if py_out[i] != ref["output"][i] and not (py_out[i] is None and ref["output"][i] is None)])

# Spot-checks
check("SMITH → smith",       py_out[0] == "smith")
check("O'Brien → obrien",    py_out[1] == "obrien")
check("jones jr. → jones",   py_out[4] == "jones")
check("WILLIAMS SR → williams", py_out[5] == "williams")
check("NA → None",           py_out[15] is None)
check("Jo-Ann → jo?ann",     py_out[16] == "jo?ann")
check("St. Claire → st?claire", py_out[17] == "st?claire")
check("de la Cruz → delacruz",  py_out[18] == "delacruz")
check("ng → ng (valid 2-char)", py_out[8] == "ng")
check("S?m?th → '' (too many ?)", py_out[12] == "")


# =============================================================================
print("\n=== 2. make_kernel ===")
# =============================================================================
ref = load("make_kernel.json")
K = ref["K"][0]
D = sp.csr_matrix(
    (ref["D_x"], (ref["D_i"], ref["D_j"])),
    shape=(K, K),
)
M = sp.csr_matrix(
    ([1.0] * len(ref["D_i"]), (ref["D_i"], ref["D_j"])),
    shape=(K, K),
)
delta  = ref["delta"][0]
lam    = ref["lambda"][0]
R_Kmat = np.array(ref["Kmat"])

Kmat_py = np.array(make_kernel(D, M, delta, lam).todense())

check("shape matches",         Kmat_py.shape == R_Kmat.shape)
check("values match (tol=1e-6)", allclose(Kmat_py, R_Kmat, tol=1e-6),
      msg=f"\nPython:\n{Kmat_py}\nR:\n{R_Kmat}")
# Isolated nodes (no neighbors) have column sum = (1-delta); connected nodes sum to 1.
# Build a boolean mask for names that have at least one neighbor (col of D is non-zero).
has_neighbor = np.asarray((D.sum(axis=0) > 0)).ravel()
expected_col_sums = np.where(has_neighbor, 1.0, 1.0 - delta)
check("column sums correct (1 if has neighbor, 1-delta if isolated)",
      allclose(Kmat_py.sum(axis=0), expected_col_sums, tol=1e-9),
      msg=f"col sums: {Kmat_py.sum(axis=0)}, expected: {expected_col_sums}")


# =============================================================================
print("\n=== 3. loglikelihood ===")
# =============================================================================
ref_ll = load("loglikelihood.json")
p5   = np.array(ref_ll["p"])
nobs = np.array(ref_ll["n_obs"])
d_ll = ref_ll["delta"][0]
l_ll = ref_ll["lambda"][0]
r_ll = ref_ll["ll"][0]

py_ll = loglikelihood(p5, D, M, d_ll, l_ll, nobs)
check("log-likelihood matches R (tol=1e-4)", allclose([py_ll], [r_ll], tol=1e-4),
      msg=f"Python={py_ll:.6f}, R={r_ll:.6f}")


# =============================================================================
print("\n=== 4. find_neighbors ===")
# =============================================================================
ref_fn = load("find_neighbors.json")
names8 = ref_fn["names"]
nm_py  = find_neighbors(names8, method="jw", max_dist=0.2, ncores=1)

D_py = nm_py["D"].tocoo()
M_py = nm_py["M"].tocoo()

# Build sorted edge sets for comparison
def edge_set(i_arr, j_arr, x_arr=None):
    if x_arr is None:
        return set(zip(i_arr, j_arr))
    return {(int(i), int(j)): float(x) for i, j, x in zip(i_arr, j_arr, x_arr)}

R_D_edges = edge_set(ref_fn["D_i"], ref_fn["D_j"], ref_fn["D_x"])
Py_D_edges = edge_set(D_py.row, D_py.col, D_py.data)

R_M_edges  = edge_set(ref_fn["M_i"], ref_fn["M_j"])
Py_M_edges = edge_set(M_py.row, M_py.col)

check("D same neighbor edges",   set(R_D_edges.keys()) == set(Py_D_edges.keys()),
      msg=f"R only: {set(R_D_edges.keys())-set(Py_D_edges.keys())}, "
          f"Py only: {set(Py_D_edges.keys())-set(R_D_edges.keys())}")
check("M same neighbor edges",   R_M_edges == Py_M_edges)

common_edges = set(R_D_edges.keys()) & set(Py_D_edges.keys())
if common_edges:
    diffs = {e: abs(R_D_edges[e] - Py_D_edges[e]) for e in common_edges}
    max_diff = max(diffs.values())
    check("D distances match R (tol=1e-6)", max_diff < 1e-6,
          msg=f"max diff={max_diff:.2e}, worst edge={max(diffs, key=diffs.get)}")


# =============================================================================
print("\n=== 5. make_bayes_choice_dictionary ===")
# =============================================================================
ref_bcd = load("make_bayes_choice_dictionary.json")
p8 = np.array(ref_bcd["p"])
# Reconstruct D from find_neighbors reference
D8 = sp.csr_matrix(
    (ref_fn["D_x"], (ref_fn["D_i"], ref_fn["D_j"])),
    shape=(8, 8),
)
bcd_py = make_bayes_choice_dictionary(
    names8, D8, p8, delta=ref_bcd["delta"][0], lambda_=ref_bcd["lambda"][0]
)

# R returns NA → Python returns None
def r_to_py(v):
    return None if v is None else v

check("observed column matches", list(bcd_py["observed"]) == ref_bcd["observed"])
check("standard column matches",
      [r_to_py(x) for x in bcd_py["standard"]] == [r_to_py(x) for x in ref_bcd["standard"]],
      msg=list(zip(bcd_py["standard"], ref_bcd["standard"])))

# p_standard — compare non-NaN entries
py_ps = bcd_py["p_standard"].values
r_ps  = np.array([float("nan") if x is None else x for x in ref_bcd["p_standard"]])
both_valid = ~np.isnan(py_ps) & ~np.isnan(r_ps)
check("p_standard matches where both non-NaN",
      bool(np.allclose(py_ps[both_valid], r_ps[both_valid], atol=1e-9)) if both_valid.any() else True)


# =============================================================================
print("\n=== 6. standardize_missing_name ===")
# =============================================================================
ref_smn = load("standardize_missing_name.json")
std_df = pd.DataFrame({
    "standard":   ref_smn["standard"],
    "p_standard": ref_smn["p_standard"],
})
result_py = standardize_missing_name(
    ref_smn["name"][0],
    std_df,
    lambda_=ref_smn["lambda"][0],
    delta=ref_smn["delta"][0],
    method=ref_smn["method"][0],
)
r_result = ref_smn["result"][0] if ref_smn["result"] else None
check(f"standardize_missing_name('smth') == R result",
      result_py == r_result,
      msg=f"Python={result_py!r}, R={r_result!r}")


# =============================================================================
print("\n=== 7. synthetic_name_counts ===")
# =============================================================================
ref_snc = load("synthetic_name_counts.json")
snc_py = synthetic_name_counts()

check("returns DataFrame",  isinstance(snc_py, pd.DataFrame))
check("has 'name' column",  "name" in snc_py.columns)
check("has 'count' column", "count" in snc_py.columns)
check("sorted descending",  snc_py["count"].is_monotonic_decreasing)
# R has same nrow (row count depends on random seed — just check same ballpark)
check(f"nrow in [100, 10000] (got {len(snc_py)})", 100 <= len(snc_py) <= 10000)
check("no duplicate names", snc_py["name"].nunique() == len(snc_py))


# =============================================================================
print("\n=== Summary ===")
# =============================================================================
if failures:
    print(f"\n{FAIL}  {len(failures)} test(s) failed:")
    for f in failures:
        print(f"    • {f}")
    sys.exit(1)
else:
    print(f"\n{PASS}  All tests passed.")
