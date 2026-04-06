"""Gibbs sampler for the name-garbling mixture model."""

import numpy as np
from typing import Callable, Optional
from tqdm import tqdm

from .make_kernel import make_kernel
from .rdirichlet import rdirichlet
from .loglikelihood import loglikelihood


def draw_gibbs(
    D,
    M,
    n_obs: np.ndarray,
    n_iter: int = 10_000,
    delta_init: float = 0.1,
    lambda_init: float = 1.0,
    sd_logit: float = 0.1,
    sd_loglam: float = 0.1,
    alpha_dir: float = 1.0,
    prior_delta: Optional[Callable[[float], float]] = None,
    prior_lambda: Optional[Callable[[float], float]] = None,
) -> dict:
    """Draw from the posterior of the name-garbling model via Gibbs sampling.

    At each iteration:

    * **p** is updated via a collapsed Dirichlet step using expected counts.
    * **(delta, lambda_)** are jointly updated via a Metropolis-Hastings
      random walk on the logit / log scale.

    Parameters
    ----------
    D : scipy.sparse matrix or numpy.ndarray
        K×K distance matrix with distances for neighbor pairs.
    M : scipy.sparse matrix or numpy.ndarray
        K×K binary neighbor mask.
    n_obs : array-like of shape (K,)
        Observed counts per name.
    n_iter : int
        Number of sampler iterations.  Default 10 000.
    delta_init : float
        Initial value for the garbling probability delta.
    lambda_init : float
        Initial value for the decay rate lambda.
    sd_logit : float
        Proposal standard deviation for logit(delta).
    sd_loglam : float
        Proposal standard deviation for log(lambda).
    alpha_dir : float or array-like of shape (K,)
        Dirichlet prior hyperparameter.  Scalar is broadcast to length K.
    prior_delta : callable, optional
        Log-density prior for delta.  Defaults to Beta(9, 1).
    prior_lambda : callable, optional
        Log-density prior for lambda.  Defaults to Gamma(shape=1, rate=0.1).

    Returns
    -------
    dict with keys:
        ``delta``   – 1-D array of sampled delta values (length n_iter).
        ``lambda_`` – 1-D array of sampled lambda values.
        ``likelihood`` – 1-D array of log-likelihood values.
        ``p_first5`` – (n_iter × min(5, K)) matrix of first-5 entries of p.
        ``p_mean``  – posterior mean of p (computed over the second half).
    """
    from scipy.stats import beta as beta_dist, gamma as gamma_dist

    n_obs = np.asarray(n_obs, dtype=float)
    K = len(n_obs)

    if np.ndim(alpha_dir) == 0:
        alpha_dir = np.full(K, float(alpha_dir))
    else:
        alpha_dir = np.asarray(alpha_dir, dtype=float)

    if prior_delta is None:
        prior_delta = lambda d: beta_dist.logpdf(d, 9, 1)
    if prior_lambda is None:
        # Gamma(shape=1, rate=0.1) → scale = 1/0.1 = 10
        prior_lambda = lambda l: gamma_dist.logpdf(l, a=1, scale=10.0)

    # Storage
    n5 = min(5, K)
    p5_chain = np.full((n_iter, n5), np.nan)
    delta_chain = np.zeros(n_iter)
    lambda_chain = np.zeros(n_iter)
    ll_chain = np.zeros(n_iter)
    p_sum = np.zeros(K)
    count_sum = 0

    # Initial state
    p_curr = n_obs / n_obs.sum()
    delta_curr = delta_init
    lambda_curr = lambda_init

    def _logit(x):
        return np.log(x / (1.0 - x))

    def _ilogit(x):
        return np.exp(x) / (1.0 + np.exp(x))

    for t in tqdm(range(n_iter), desc="draw_gibbs"):
        # (1) Collapsed Dirichlet update for p
        Kmat = make_kernel(D, M, delta_curr, lambda_curr)
        p_arr = np.asarray(p_curr).ravel()
        denom = np.asarray(Kmat @ p_arr, dtype=float).ravel()
        tmp = n_obs / denom
        exp_counts = p_arr * np.asarray(Kmat.T @ tmp, dtype=float).ravel()
        p_curr = rdirichlet(alpha_dir + exp_counts)

        p5_chain[t] = p_curr[:n5]

        if t >= n_iter // 2:
            p_sum += p_curr
            count_sum += 1

        # (2) Joint MH step for (delta, lambda_)
        logit_prop = np.random.normal(_logit(delta_curr), sd_logit)
        d_prop = _ilogit(logit_prop)
        logl_prop = np.random.normal(np.log(lambda_curr), sd_loglam)
        lam_prop = np.exp(logl_prop)

        ll_prop = loglikelihood(p_curr, D, M, d_prop, lam_prop, n_obs)
        ll_curr = loglikelihood(p_curr, D, M, delta_curr, lambda_curr, n_obs)

        log_post_curr = ll_curr + prior_delta(delta_curr) + prior_lambda(lambda_curr)
        log_post_prop = ll_prop + prior_delta(d_prop) + prior_lambda(lam_prop)

        # Jacobian corrections for logit / log transforms
        jac_delta = np.log(d_prop * (1.0 - d_prop)) - np.log(delta_curr * (1.0 - delta_curr))
        jac_lambda = np.log(lam_prop) - np.log(lambda_curr)

        log_r = (log_post_prop - log_post_curr) + jac_delta + jac_lambda

        if np.log(np.random.uniform()) < log_r:
            delta_curr = d_prop
            lambda_curr = lam_prop
            ll_curr = ll_prop

        delta_chain[t] = delta_curr
        lambda_chain[t] = lambda_curr
        ll_chain[t] = ll_curr

    return {
        "delta": delta_chain,
        "lambda_": lambda_chain,
        "likelihood": ll_chain,
        "p_first5": p5_chain,
        "p_mean": p_sum / count_sum if count_sum > 0 else p_sum,
    }
